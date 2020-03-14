use crate::unification::UnifyingPat;
use std::fmt::{Debug, Display, Error, Formatter};
use std::ops::{Add, BitAnd, BitOr, Div, Mul, Not, Sub, Neg};
use std::rc::Rc;
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
    Arr(Vec<Self>, Option<Name>),
    // ?x + ?y * 2
    Eval(Evaluation<Box<Self>>),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Evaluation<T> {
    Add(T, T),
    Sub(T, T),
    Mul(T, T),
    Div(T, T),
    Pos(T),
    Neg(T),
    Append(T, T),
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
    pub(crate) fn to_unify(&self) -> Rc<UnifyingPat> {
        Rc::new(match self {
            Pat::Num(n) => UnifyingPat::Num(*n),
            Pat::Str(s) => UnifyingPat::Str(s.clone()),
            Pat::Var(v) => UnifyingPat::Var(Rc::new(v.clone())),
            Pat::Arr(arr, v) => UnifyingPat::Slice(
                arr.iter().map(Pat::to_unify).collect(),
                v.clone().map(Rc::new),
            ),
            Pat::Eval(e) => UnifyingPat::Eval(e.clone().map(|x| x.to_unify())),
        })
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

    pub fn expand_to(self, q: Term) -> Decl {
        Decl {
            pat: self,
            expanded: q,
        }
    }

    pub fn datum(self) -> Decl {
        self.expand_to(unit())
    }

    pub fn append(self, rhs: Self) -> Self {
        Pat::Eval(Evaluation::Append(Box::new(self), Box::new(rhs)))
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
            Pat::Eval(e) => write!(f, "{}", e),
        }
    }
}

impl Add for Pat {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Pat::Eval(Evaluation::Add(Box::new(self), Box::new(rhs)))
    }
}

impl Sub for Pat {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Pat::Eval(Evaluation::Sub(Box::new(self), Box::new(rhs)))
    }
}

impl Mul for Pat {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        Pat::Eval(Evaluation::Mul(Box::new(self), Box::new(rhs)))
    }
}

impl Div for Pat {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        Pat::Eval(Evaluation::Div(Box::new(self), Box::new(rhs)))
    }
}

impl Neg for Pat {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Pat::Eval(Evaluation::Neg(Box::new(self)))
    }
}

impl<T> Evaluation<T> {
    pub fn map<U, F>(self, mut f: F) -> Evaluation<U>
    where
        F: FnMut(T) -> U,
    {
        match self {
            Evaluation::Add(x, y) => Evaluation::Add(f(x), f(y)),
            Evaluation::Sub(x, y) => Evaluation::Sub(f(x), f(y)),
            Evaluation::Mul(x, y) => Evaluation::Mul(f(x), f(y)),
            Evaluation::Div(x, y) => Evaluation::Div(f(x), f(y)),
            Evaluation::Append(x, y) => Evaluation::Append(f(x), f(y)),
            Evaluation::Pos(x) => Evaluation::Pos(f(x)),
            Evaluation::Neg(x) => Evaluation::Neg(f(x))
        }
    }
}

impl<T: Display> Display for Evaluation<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            Evaluation::Add(x, y) => write!(f, "({} + {})", x, y),
            Evaluation::Sub(x, y) => write!(f, "({} - {})", x, y),
            Evaluation::Mul(x, y) => write!(f, "({} * {})", x, y),
            Evaluation::Div(x, y) => write!(f, "({} / {})", x, y),
            Evaluation::Append(x, y) => write!(f, "({} <> {})", x, y),
            Evaluation::Pos(x) => write!(f, "+{}", x),
            Evaluation::Neg(x) => write!(f, "-{}", x),
        }
    }
}

impl Term {
    fn rename(&self, id: &str) -> Self {
        match self {
            Term::Unit => Term::Unit,
            Term::Simple(p) => Term::Simple(p.rename(id)),
            Term::Conjoin(p, q) => Term::Conjoin(Box::new(p.rename(id)), Box::new(q.rename(id))),
            Term::Disjoint(p, q) => Term::Disjoint(Box::new(p.rename(id)), Box::new(q.rename(id))),
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
            Term::Unit => f.write_str("unit"),
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
            (Term::Conjoin(p1, q1), Term::Conjoin(p2, q2))
            | (Term::Disjoint(p1, q1), Term::Disjoint(p2, q2)) => p1.eq(p2) && q1.eq(q2),
            (Term::Negative(n), Term::Negative(m)) => n.eq(m),
            _ => false,
        }
    }
}

impl Eq for Term {}

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
            expanded: body,
        }
    }
}

impl Display for Decl {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "{} => {}", self.pat, self.expanded)
    }
}
