use std::fmt::{Display, Error, Formatter};
use std::ops::{BitAnd, BitOr, Not};
use uuid::Uuid;
use std::rc::Rc;

pub type Name = String;

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Pat {
    Num(i64),                     // 123
    Str(String),                  // "string"
    Var(Name),                    // ?x
    Comp(Vec<Rc<Pat>>, Option<Name>), // [prefix, ?x..]
}

/**
 [?x, ?x..] ~ [[1,2,3], 1, 2, 3]
 x : [1,2,3]
*/

impl Pat {
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

pub fn comp(ps: Vec<Pat>) -> Pat {
    use Pat::*;
    Comp(ps.into_iter().map(Rc::new).collect(), None)
}

pub fn comp_rest(ps: Vec<Pat>, v: &str) -> Pat {
    use Pat::*;
    Comp(ps.into_iter().map(Rc::new).collect(), Some(v.to_string()))
}

impl Display for Pat {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            Pat::Num(n) => write!(f, "{}", n),
            Pat::Str(s) => write!(f, "\"{}\"", s),
            Pat::Var(v) => write!(f, "?{}", v),
            Pat::Comp(ps, None) => {
                f.write_str("[")?;
                if ps.is_empty() {
                    return f.write_str("]");
                }

                for p in ps.iter().take(ps.len() - 1) {
                    write!(f, "{}, ", p)?;
                }
                write!(f, "{}]", ps.last().unwrap())
            }
            Pat::Comp(ps, Some(v)) => {
                f.write_str("[")?;
                for p in ps {
                    write!(f, "{}, ", p)?;
                }
                write!(f, "?{}...]", v)
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum Query {
    True,
    Simple(Pat),
    Conjoin(Box<Self>, Box<Self>),
    Disjoint(Box<Self>, Box<Self>),
    Negative(Box<Self>),
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

impl Query {
    pub fn and(self, rhs: Self) -> Self {
        self & rhs
    }

    pub fn or(self, rhs: Self) -> Self {
        self | rhs
    }

    pub fn not(self) -> Self {
        !self
    }
}

pub fn qtrue() -> Query {
    Query::True
}

impl Display for Query {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            Query::True => f.write_str("true"),
            Query::Simple(p) => write!(f, "{}", p),
            Query::Conjoin(p, q) => write!(f, "({} && {})", p, q),
            Query::Disjoint(p, q) => write!(f, "({} || {})", p, q),
            Query::Negative(n) => write!(f, "~{}", n),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Assertion {
    pub conclusion: Pat,
    pub body: Query,
}

impl Assertion {
    pub fn rename(&self) -> Self {
        fn walk_pat(x: &Pat, id: &str) -> Pat {
            match x {
                Pat::Var(v) => Pat::Var(v.clone() + id),
                Pat::Comp(ps, s) => Pat::Comp(
                    ps.iter().map(|p| walk_pat(p, id)).map(Rc::new).collect(),
                    s.clone().map(|v| v + id),
                ),
                pat => pat.clone(),
            }
        }

        fn walk_query(x: &Query, id: &str) -> Query {
            match x {
                Query::True => Query::True,
                Query::Simple(pat) => Query::Simple(walk_pat(pat, id)),
                Query::Conjoin(p, q) => {
                    Query::Conjoin(Box::new(walk_query(p, id)), Box::new(walk_query(q, id)))
                }
                Query::Disjoint(p, q) => {
                    Query::Disjoint(Box::new(walk_query(p, id)), Box::new(walk_query(q, id)))
                }
                Query::Negative(q) => Query::Negative(Box::new(walk_query(q, id))),
            }
        }

        fn walk(
            Assertion {
                conclusion: conclude,
                body,
            }: &Assertion,
            id: &str,
        ) -> Assertion {
            Assertion {
                conclusion: walk_pat(conclude, id),
                body: walk_query(body, id),
            }
        }

        let uid = Uuid::new_v4().to_string();
        walk(self, &uid)
    }
}
