use crate::unification::UnifyingPat;
use std::fmt::{Display, Error, Formatter};
use std::ops::{BitAnd, BitOr, Not};
use uuid::Uuid;

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

impl Pat {
    pub(crate) fn to_unify(&self) -> UnifyingPat {
        match self {
            Pat::Num(n) => UnifyingPat::Num(n),
            Pat::Str(s) => UnifyingPat::Str(s),
            Pat::Var(v) => UnifyingPat::Var(v),
            Pat::Arr(arr, v) => UnifyingPat::Slice(arr, v.as_ref()),
        }
    }

    pub(crate) fn rename(&self, id: &str) -> Self {
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
