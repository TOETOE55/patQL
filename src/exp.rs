use crate::exp::InstantiateErr::{UnexpectedPattern, UnsolvedVariable};
use crate::exp::UnifyErr::{CannotUnify, CycleDependency};
use bumpalo::Bump;
use std::collections::HashMap;
use std::fmt::{Display, Error, Formatter};
use std::iter::once;
use std::ops::{BitAnd, BitOr, Not};
use std::slice::Iter;
use uuid::Uuid;

/****************************************************************
**                         AST
*****************************************************************/

pub type Name = String;

#[derive(Clone, Debug)]
pub enum Pat {
    Num(i64),
    // 123
    Str(String),
    // "string"
    Var(Name),
    // ?x
    Arr(Vec<Pat>, Option<Name>), // [prefix, ?x..]
}

#[derive(Clone)]
pub enum Query {
    True,
    // true
    Simple(Pat),
    // [a, ?x, b]
    Conjoin(Box<Self>, Box<Self>),
    // p && q
    Disjoint(Box<Self>, Box<Self>),
    // p || q
    Negative(Box<Self>),
    // ~p
    Apply(String, fn(&Pat) -> bool, Pat), // (>100) ?x
}

#[derive(Clone)]
pub struct Assertion {
    pub conclusion: Pat,
    pub body: Query,
}

pub fn var(v: &str) -> Pat {
    use Pat::*;
    Var(v.to_string())
}

pub fn string(s: &str) -> Pat {
    use Pat::*;
    Str(s.to_string())
}

pub fn num(n: i64) -> Pat {
    use Pat::*;
    Num(n)
}

pub fn arr(ps: Vec<Pat>) -> Pat {
    use Pat::*;
    Arr(ps, None)
}

pub fn slice(ps: Vec<Pat>, v: &str) -> Pat {
    use Pat::*;
    Arr(ps, Some(v.to_string()))
}

pub fn qtrue() -> Query {
    Query::True
}

/****************************************************************
**                        Some Data
*****************************************************************/

#[derive(Copy, Clone, Debug)]
pub enum UnifyingPat<'a> {
    Num(&'a i64),
    Str(&'a str),
    Var(&'a Name),
    Slice(&'a [Pat], Option<&'a Name>),
}

#[derive(Clone, Debug, Default)]
pub struct Dict<'a> {
    dict: HashMap<&'a str, UnifyingPat<'a>>,
}

pub struct Assertions {
    assertions: Vec<Assertion>,
}

pub enum DictStream<'a, I> {
    True(I),
    Simple(&'a Pat, &'a Assertions, &'a Bump, I),
    Conjoin(Box<DictStream<'a, DictStream<'a, I>>>),
    //    Disjoint(Dup<Box<DictStream<'a, I>>>),
    Negative(Box<DictStream<'a, I>>),
    Apply(fn(&Pat) -> bool, &'a Pat, I),
}

#[derive(Clone, Copy, Debug)]
pub enum UnifyErr<'a> {
    CannotUnify(UnifyingPat<'a>, UnifyingPat<'a>),
    CycleDependency(UnifyingPat<'a>, UnifyingPat<'a>),
}

pub type UnifyResult<'a, T = ()> = Result<T, UnifyErr<'a>>;

#[derive(Clone, Debug)]
pub enum InstantiateErr {
    UnsolvedVariable(Name),
    UnexpectedPattern(Pat),
}

pub type InstantiateResult<T = ()> = Result<T, InstantiateErr>;

/****************************************************************
**                         Impl
*****************************************************************/

impl Pat {
    pub fn to_unify(&self) -> UnifyingPat {
        match self {
            Pat::Num(n) => UnifyingPat::Num(n),
            Pat::Str(s) => UnifyingPat::Str(s),
            Pat::Var(v) => UnifyingPat::Var(v),
            Pat::Arr(arr, v) => UnifyingPat::Slice(arr, v.as_ref()),
        }
    }

    pub fn rename(&self, id: &str) -> Self {
        match self {
            Pat::Var(v) => Pat::Var(v.clone() + id),
            Pat::Arr(ps, v) => Pat::Arr(
                ps.iter().map(|p| p.rename(id)).collect(),
                v.clone().map(|v| v + id),
            ),
            pat => pat.clone(),
        }
    }

    pub fn q(self) -> Query {
        Query::Simple(self)
    }

    pub fn assert_as(self, q: Query) -> Assertion {
        Assertion {
            conclusion: self,
            body: q,
        }
    }

    pub fn datum(self) -> Assertion {
        self.assert_as(qtrue())
    }
}

impl Display for Pat {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            Pat::Num(n) => write!(f, "{}", n),
            Pat::Str(s) => write!(f, "\"{}\"", s),
            Pat::Var(v) => write!(f, "?{}", v),
            Pat::Arr(ps, None) => {
                f.write_str("[")?;
                if ps.is_empty() {
                    return f.write_str("]");
                }

                for p in ps.iter().take(ps.len() - 1) {
                    write!(f, "{}, ", p)?;
                }
                write!(f, "{}]", ps.last().unwrap())
            }
            Pat::Arr(ps, Some(v)) => {
                f.write_str("[")?;
                for p in ps {
                    write!(f, "{}, ", p)?;
                }
                write!(f, "?{}...]", v)
            }
        }
    }
}

impl<'a> Dict<'a> {
    pub fn get(&self, var: &str) -> Option<UnifyingPat<'a>> {
        self.dict.get(var).copied()
    }

    pub fn insert(&mut self, var: &'a str, val: UnifyingPat<'a>) -> Option<UnifyingPat<'a>> {
        self.dict.insert(var, val)
    }

    pub fn unify(&mut self, p: &'a Pat, q: &'a Pat) -> UnifyResult {
        unify(p.to_unify(), q.to_unify(), self)
    }

    pub fn subst(&self, pat: &Pat) -> InstantiateResult<Pat> {
        substitute(pat, self)
    }

    pub fn inst(&self, q: &Query) -> InstantiateResult<Query> {
        instantiate(q, self)
    }
}

impl UnifyingPat<'_> {
    pub fn unified(self) -> Pat {
        match self {
            UnifyingPat::Num(n) => Pat::Num(*n),
            UnifyingPat::Str(s) => Pat::Str(s.to_string()),
            UnifyingPat::Var(v) => Pat::Var(v.to_owned()),
            UnifyingPat::Slice(ps, v) => Pat::Arr(ps.to_vec(), v.cloned()),
        }
    }
}

impl Query {
    fn rename(&self, id: &str) -> Self {
        match self {
            Query::True => Query::True,
            Query::Simple(p) => Query::Simple(p.rename(id)),
            Query::Conjoin(p, q) => Query::Conjoin(Box::new(p.rename(id)), Box::new(q.rename(id))),
            Query::Disjoint(p, q) => {
                Query::Disjoint(Box::new(p.rename(id)), Box::new(q.rename(id)))
            }
            Query::Negative(q) => Query::Negative(Box::new(q.rename(id))),
            Query::Apply(d, f, p) => Query::Apply(d.clone(), *f, p.rename(id)),
        }
    }
}

impl Not for Query {
    type Output = Self;

    fn not(self) -> Self::Output {
        Self::Negative(Box::new(self))
    }
}

impl BitAnd for Query {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        Self::Conjoin(Box::new(self), Box::new(rhs))
    }
}

impl BitOr for Query {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        Self::Disjoint(Box::new(self), Box::new(rhs))
    }
}

impl Display for Query {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            Query::True => f.write_str("true"),
            Query::Simple(p) => write!(f, "{}", p),
            Query::Conjoin(p, q) => write!(f, "({} && {})", p, q),
            Query::Disjoint(p, q) => write!(f, "({} || {})", p, q),
            Query::Negative(n) => write!(f, "~{}", n),
            Query::Apply(d, _, p) => write!(f, "({} {})", d, p),
        }
    }
}

impl Assertion {
    pub fn rename(&self) -> Self {
        let id = Uuid::new_v4().to_string();
        Self {
            conclusion: self.conclusion.rename(&id),
            body: self.body.rename(&id),
        }
    }
}

impl Assertions {
    pub fn new(assertions: Vec<Assertion>) -> Self {
        Self { assertions }
    }

    pub fn connect(&self) -> Iter<Assertion> {
        self.assertions.iter()
    }
}

/****************************************************************
**                         Unify
*****************************************************************/

pub fn unify<'a>(p: UnifyingPat<'a>, q: UnifyingPat<'a>, dict: &mut Dict<'a>) -> UnifyResult<'a> {
    use UnifyingPat::*;
    match (p, q) {
        (Num(m), Num(n)) if m == n => Ok(()),
        (Str(s1), Str(s2)) if s1 == s2 => Ok(()),
        (v @ Var(_), p) | (p, v @ Var(_)) => solve(v, p, dict),
        (Slice(ps, None), Slice(qs, None)) if ps.len() == qs.len() => {
            for (p, q) in ps.iter().zip(qs) {
                unify(p.to_unify(), q.to_unify(), dict)?;
            }
            Ok(())
        }
        (Slice(ps, Some(v)), Slice(qs, s)) | (Slice(qs, s), Slice(ps, Some(v)))
            if ps.len() <= qs.len() =>
        {
            let (qs_prefix, qs_suffix) = qs.split_at(ps.len());
            for (p, q) in ps.iter().zip(qs_prefix) {
                unify(p.to_unify(), q.to_unify(), dict)?;
            }
            solve(Var(v), Slice(qs_suffix, s), dict)
        }
        _ => Err(CannotUnify(p, q)),
    }
}

fn solve<'a>(var: UnifyingPat<'a>, val: UnifyingPat<'a>, dict: &mut Dict<'a>) -> UnifyResult<'a> {
    use UnifyingPat::*;

    let name = match var {
        Var(v) => v,
        _ => panic!("expected Var"),
    };

    if let Some(bind) = dict.get(name) {
        return unify(bind, val, dict);
    }

    if let Var(v) = val {
        if let Some(bind) = dict.get(v) {
            return unify(var, bind, dict);
        }
        if v != name {
            dict.insert(name, val);
        }

        return Ok(());
    }

    if depends_on(val, name, dict) {
        return Err(CycleDependency(var, val));
    }

    dict.insert(name, val);
    Ok(())
}

pub fn depends_on(val: UnifyingPat, var: &str, dict: &Dict) -> bool {
    use UnifyingPat::*;
    match val {
        Var(v) => v == var || !dict.get(v).into_iter().all(|p| !depends_on(p, var, dict)),
        Slice(pats, slice) => {
            !pats.iter().all(|p| !depends_on(p.to_unify(), var, dict))
                || slice.as_ref().map_or(false, |v| {
                    *v == var || !dict.get(v).into_iter().all(|p| !depends_on(p, var, dict))
                })
        }
        _ => false,
    }
}

/****************************************************************
**                         Instantiate
*****************************************************************/

pub fn substitute(pat: &Pat, dict: &Dict) -> InstantiateResult<Pat> {
    Ok(match pat {
        Pat::Var(v) => {
            let pat = dict
                .get(v)
                .ok_or_else(|| UnsolvedVariable(v.clone()))?
                .unified();
            substitute(&pat, dict)?
        }
        Pat::Arr(ps, v) => {
            let mut collect = vec![];
            for p in ps {
                collect.push(substitute(p, dict)?);
            }

            if let Some(v) = v {
                let p = dict.get(v).ok_or_else(|| UnsolvedVariable(v.clone()))?;
                match substitute(&p.unified(), dict)? {
                    Pat::Arr(mut ps, None) => collect.append(&mut ps),
                    Pat::Arr(_, Some(v)) => return Err(UnsolvedVariable(v)),
                    p => return Err(UnexpectedPattern(p)),
                }
            }

            Pat::Arr(collect, None)
        }
        p => p.clone(),
    })
}

pub fn instantiate(q: &Query, dict: &Dict) -> InstantiateResult<Query> {
    Ok(match q {
        Query::True => Query::True,
        Query::Simple(p) => Query::Simple(substitute(p, dict)?),
        Query::Conjoin(p, q) => Query::Conjoin(
            Box::new(instantiate(p, dict)?),
            Box::new(instantiate(q, dict)?),
        ),
        Query::Disjoint(p, q) => Query::Disjoint(
            Box::new(instantiate(p, dict)?),
            Box::new(instantiate(q, dict)?),
        ),
        Query::Negative(q) => instantiate(q, dict)?,
        Query::Apply(d, f, p) => Query::Apply(d.clone(), *f, substitute(p, dict)?),
    })
}

/****************************************************************
**                         Evaluation
*****************************************************************/

pub fn run_query(q: Query, asserts: &Assertions) -> Vec<Query> {
    let arena = Bump::new();
    qeval(&q, asserts, &arena, once(Dict::default())).fold(vec![], |mut acc, dict| {
        if let Ok(q) = dict.inst(&q) {
            acc.push(q)
        }
        acc
    })
}

pub fn qeval<'a, I>(
    q: &'a Query,
    asserts: &'a Assertions,
    arena: &'a Bump,
    dicts: I,
) -> Box<dyn Iterator<Item = Dict<'a>> + 'a>
where
    I: Iterator<Item = Dict<'a>> + 'a,
{
    match q {
        Query::True => Box::new(dicts),
        Query::Simple(p) => Box::new(apply_asserts(p, asserts, arena, dicts)),
        Query::Conjoin(p, q) => qeval(p, asserts, arena, qeval(q, asserts, arena, dicts)),
        Query::Disjoint(p, q) => Box::new(dicts.flat_map(move |dict| {
            qeval(p, asserts, arena, once(dict.clone())).chain(qeval(q, asserts, arena, once(dict)))
        })),
        Query::Negative(p) => Box::new(dicts.flat_map(move |dict| {
            let v = qeval(p, asserts, arena, once(dict.clone())).collect::<Vec<_>>();
            if v.is_empty() {
                Some(dict)
            } else {
                None
            }
        })),
        Query::Apply(_, f, p) => Box::new(dicts.filter(move |dict| {
            if let Ok(p) = dict.subst(p) {
                f(&p)
            } else {
                false
            }
        })),
    }
}

pub fn apply_asserts<'a, I>(
    pat: &'a Pat,
    asserts: &'a Assertions,
    arena: &'a Bump,
    dicts: I,
) -> impl Iterator<Item = Dict<'a>> + 'a
where
    I: Iterator<Item = Dict<'a>> + 'a,
{
    dicts.flat_map(move |dict| {
        asserts.connect().flat_map(move |asst| {
            let mut dict = dict.clone();
            let renamed = arena.alloc(asst.rename());
            if let Ok(()) = dict.unify(pat, &renamed.conclusion) {
                qeval(&renamed.body, asserts, arena, once(dict))
            } else {
                Box::new(None.into_iter())
            }
        })
    })
}
