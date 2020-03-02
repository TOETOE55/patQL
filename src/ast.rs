use crate::unification::UnifyingPat;
use std::fmt::{Display, Error, Formatter, Debug};
use std::ops::{BitAnd, BitOr, Not};
use uuid::Uuid;

pub type Name = String;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Pat {
    // 123
    Num(i64),
    // "string"
    Str(String),
    // ?x
    Var(Name),
    // [prefix, ...?x]
    Arr(Vec<Pat>, Option<Name>),
    // ?x + ?y * 2
    Eval(Op, Vec<Pat>),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Op {
    Plus,
    Minus,
    Mult,
    Divi,
    Append,
}

#[derive(Clone)]
pub enum Term {
    // success
    Unit,
    // [a, ?x, b]
    Simple(Pat),
    // p && q
    Conjoin(Box<Self>, Box<Self>),
    // p || q
    Disjoint(Box<Self>, Box<Self>),
    // ~p
    Negative(Box<Self>),
    // (>100) ?x
    Filter(String, fn(&Pat) -> bool, Pat),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Decl {
    pub pat: Pat,
    pub expanded: Term,
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

pub fn unit() -> Term {
    Term::Unit
}

impl Pat {
    pub(crate) fn to_unify(&self) -> UnifyingPat {
        match self {
            Pat::Num(n) => UnifyingPat::Num(n),
            Pat::Str(s) => UnifyingPat::Str(s),
            Pat::Var(v) => UnifyingPat::Var(v),
            Pat::Arr(arr, v) => UnifyingPat::Slice(arr, v.as_ref()),
            Pat::Eval(_, _) => unimplemented!(),
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

    pub fn q(self) -> Term {
        Term::Simple(self)
    }

    pub fn filter(self, decr: &str, f: fn(&Pat) -> bool) -> Term {
        Term::Filter(decr.to_string(), f, self)
    }

    pub fn expend_to(self, q: Term) -> Decl {
        Decl {
            pat: self,
            expanded: q,
        }
    }

    pub fn datum(self) -> Decl {
        self.expend_to(unit())
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
                write!(f, "...?{}]", v)
            }
            Pat::Eval(_, _) => unimplemented!(),
        }
    }
}

impl Term {
    fn rename(&self, id: &str) -> Self {
        match self {
            Term::Unit => Term::Unit,
            Term::Simple(p) => Term::Simple(p.rename(id)),
            Term::Conjoin(p, q) => {
                Term::Conjoin(Box::new(p.rename(id)), Box::new(q.rename(id)))
            }
            Term::Disjoint(p, q) => {
                Term::Disjoint(Box::new(p.rename(id)), Box::new(q.rename(id)))
            }
            Term::Negative(q) => Term::Negative(Box::new(q.rename(id))),
            Term::Filter(d, f, p) => Term::Filter(d.clone(), *f, p.rename(id)),
        }
    }

    fn and(self, rhs: Self) -> Self {
        self & rhs
    }

    fn or(self, rhs: Self) -> Self {
        self | rhs
    }
}

impl Not for Term {
    type Output = Self;

    fn not(self) -> Self::Output {
        Self::Negative(Box::new(self))
    }
}

impl BitAnd for Term {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        Self::Conjoin(Box::new(self), Box::new(rhs))
    }
}

impl BitOr for Term {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        Self::Disjoint(Box::new(self), Box::new(rhs))
    }
}

impl Display for Term {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            Term::Unit => f.write_str("true"),
            Term::Simple(p) => write!(f, "{}", p),
            Term::Conjoin(p, q) => write!(f, "({} & {})", p, q),
            Term::Disjoint(p, q) => write!(f, "({} | {})", p, q),
            Term::Negative(n) => write!(f, "~{}", n),
            Term::Filter(d, _, p) => write!(f, "({} {})", d, p),
        }
    }
}

impl Debug for Term {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "{}", self)
    }
}

impl PartialEq for Term {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Term::Unit, Term::Unit) => true,
            (Term::Simple(p), Term::Simple(q)) => p.eq(q),
            (Term::Conjoin(p1,q1), Term::Conjoin(p2,q2)) |
            (Term::Disjoint(p1,q1), Term::Disjoint(p2,q2)) => p1.eq(p2) && q1.eq(q2),
            (Term::Negative(n), Term::Negative(m)) => n.eq(m),
            _ => false,
        }
    }
}

impl Eq for Term {

}

impl Decl {
    pub fn rename(&self) -> Self {
        let id = Uuid::new_v4().to_string();
        Self {
            pat: self.pat.rename(&id),
            expanded: self.expanded.rename(&id),
        }
    }

    pub fn new(pat: Pat, body: Term) -> Self {
        Self {
            pat,
            expanded: body
        }
    }
}
