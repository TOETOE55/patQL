use crate::ast::{Name, Pat, Query};
use crate::qeval::Dict;
use crate::unify::UnifyErr::{CannotUnify, CycleDependency, UnexpectedPattern, UnsolvedVariable};
use std::rc::Rc;

pub fn unify(p: Rc<Pat>, q: Rc<Pat>, dict: &mut Dict) -> UnifyResult<()> {
    use Pat::*;
    match (&*p, &*q) {
        (Num(m), Num(n)) if m == n => Ok(()),
        (Str(s1), Str(s2)) if s1 == s2 => Ok(()),
        (Comp(ps, None), Comp(qs, None)) if ps.len() == qs.len() => {
            for (p, q) in ps.iter().zip(qs) {
                unify(p.clone(), q.clone(), dict)?;
            }
            Ok(())
        }
        (Comp(ps, Some(v)), Comp(qs, s)) | (Comp(qs, s), Comp(ps, Some(v)))
            if ps.len() <= qs.len() =>
        {
            let (qs_prefix, qs_suffix) = qs.split_at(ps.len());
            for (p, q) in ps.iter().zip(qs_prefix) {
                unify(p.clone(), q.clone(), dict)?;
            }
            solve(Rc::new(Var(v.clone())), Rc::new(Comp(qs_suffix.to_vec(), s.clone())), dict)
        },
        (Var(_), _)  => solve(p, q, dict),
        (_, Var(_))  => solve(q, p, dict),
        _ => Err(CannotUnify((*p).clone(), (*q).clone())),
    }
}

#[derive(Clone, Debug)]
pub enum UnifyErr {
    CannotUnify(Pat, Pat),
    CycleDependency(Pat, Pat),
    UnexpectedPattern(Pat),
    UnsolvedVariable(String),
}

pub type UnifyResult<T> = Result<T, UnifyErr>;

pub fn substitute(pat: &Pat, dict: &Dict) -> UnifyResult<Pat> {
    match pat {
        Pat::Num(n) => Ok(Pat::Num(*n)),
        Pat::Str(s) => Ok(Pat::Str(s.clone())),
        Pat::Var(v) => {
            let p = dict.get(v).ok_or(UnsolvedVariable(v.clone()))?;
            substitute(&p, dict)
        }
        Pat::Comp(ps, v) => {
            let mut collect = vec![];
            for p in ps {
                collect.push(Rc::new(substitute(p, dict)?));
            }

            if let Some(v) = v {
                let p = dict.get(v).ok_or(UnsolvedVariable(v.clone()))?;
                match substitute(&p, dict)? {
                    Pat::Comp(mut ps, None) => collect.append(&mut ps),
                    Pat::Comp(_, Some(v)) => return Err(UnsolvedVariable(v)),
                    p => return Err(UnexpectedPattern(p.clone())),
                }
            }

            Ok(Pat::Comp(collect, None))
        }
    }
}

pub fn instantiate(q: &Query, dict: &Dict) -> UnifyResult<Query> {
    match q {
        Query::True => Ok(Query::True),
        Query::Simple(p) => Ok(Query::Simple(substitute(p, dict)?)),
        Query::Conjoin(p, q) => Ok(Query::Conjoin(
            Box::new(instantiate(p, dict)?),
            Box::new(instantiate(q, dict)?),
        )),
        Query::Disjoint(p, q) => Ok(Query::Disjoint(
            Box::new(instantiate(p, dict)?),
            Box::new(instantiate(q, dict)?),
        )),
        Query::Negative(p) => Ok(Query::Negative(Box::new(instantiate(p, dict)?))),
    }
}

fn solve(var: Rc<Pat>, val: Rc<Pat>, dict: &mut Dict) -> UnifyResult<()> {
    use Pat::*;

    let name = match &*var {
        Var(v) => v,
        _ => panic!("expected Var"),
    };

    if let Some(bind) = dict.get(name) {
        return unify(bind, val, dict);
    }

    if let Var(v) = &*val {
        if let Some(bind) = dict.get(v) {
            return unify(var, bind, dict);
        }
        if v != name {
            dict.insert(name.clone(), val);
        }

        return Ok(());
    }

    if depends_on(&val, name, dict) {
        return Err(CycleDependency((*var).clone(), (*val).clone()));
    }

    dict.insert(name.clone(), val.clone());
    Ok(())
}

pub fn depends_on(val: &Pat, var: &Name, dict: &Dict) -> bool {
    use Pat::*;
    match val {
        Var(v) => v == var || !dict.get(v).into_iter().all(|p| !depends_on(&p, var, dict)),
        Comp(pats, slice) => {
            !pats.iter().all(|p| !depends_on(p, var, dict))
                || slice.as_ref().map_or(false, |v| {
                    v == var || !dict.get(v).into_iter().all(|p| !depends_on(&p, var, dict))
                })
        }
        _ => false,
    }
}
