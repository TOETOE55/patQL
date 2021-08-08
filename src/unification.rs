use crate::ast::{Evaluation, Name, Pat};
use crate::evaluation::Dict;
use crate::instantiation::substitute;
use crate::unification::UnifyErr::{CannotReduce, CannotUnify, CycleDependency};
use std::rc::Rc;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum UnifyingPat {
    Num(i64),
    Str(String),
    Var(Rc<Name>),
    Slice(Vec<Rc<Self>>, Option<Rc<Name>>),
    Eval(Evaluation<Rc<Self>>),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum UnifyErr {
    CannotUnify(Rc<UnifyingPat>, Rc<UnifyingPat>),
    CycleDependency(Rc<UnifyingPat>, Rc<UnifyingPat>),
    CannotReduce(Rc<UnifyingPat>),
}

pub type UnifyResult<T = ()> = Result<T, UnifyErr>;

impl UnifyingPat {
    pub fn unified(&self) -> Pat {
        match self {
            UnifyingPat::Num(n) => Pat::Num(*n),
            UnifyingPat::Str(s) => Pat::Str(s.to_string()),
            UnifyingPat::Var(v) => Pat::Var((**v).to_owned()),
            UnifyingPat::Slice(ps, v) => Pat::Arr(
                ps.iter().map(|x| x.unified()).collect(),
                v.clone().map(|x| (*x).clone()),
            ),
            UnifyingPat::Eval(e) => Pat::Eval(e.clone().map(|x| Box::new(x.unified()))),
        }
    }
}

pub(crate) fn unify(p: Rc<UnifyingPat>, q: Rc<UnifyingPat>, dict: &mut Dict) -> UnifyResult {
    use UnifyingPat::*;
    match (&*p, &*q) {
        (Num(m), Num(n)) if m == n => Ok(()),
        (Str(s1), Str(s2)) if s1 == s2 => Ok(()),
        (Slice(ps, None), Slice(qs, None)) if ps.len() == qs.len() => {
            for (p, q) in ps.iter().zip(qs) {
                unify(p.clone(), q.clone(), dict)?;
            }
            Ok(())
        }
        (Slice(ps, Some(v)), Slice(qs, s)) | (Slice(qs, s), Slice(ps, Some(v)))
            if ps.len() <= qs.len() =>
        {
            let (qs_prefix, qs_suffix) = qs.split_at(ps.len());
            for (p, q) in ps.iter().zip(qs_prefix) {
                unify(p.clone(), q.clone(), dict)?;
            }
            solve(
                Rc::new(Var(v.clone())),
                Rc::new(Slice(qs_suffix.to_vec(), s.clone())),
                dict,
            )
        }
        (Var(_), _) => solve(p, q, dict),
        (_, Var(_)) => solve(q, p, dict),
        (Eval(..), _) => unify(normalize(p, dict).map(Rc::new)?, q, dict),
        (_, Eval(..)) => unify(p, normalize(q, dict).map(Rc::new)?, dict),
        _ => Err(CannotUnify(p, q)),
    }
}

fn solve(var: Rc<UnifyingPat>, val: Rc<UnifyingPat>, dict: &mut Dict) -> UnifyResult {
    use UnifyingPat::*;

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
        return Err(CycleDependency(var, val));
    }

    dict.insert(name.clone(), val);
    Ok(())
}

pub fn depends_on(val: &UnifyingPat, var: &str, dict: &Dict) -> bool {
    use UnifyingPat::*;
    match val {
        Var(v) => v.as_str() == var || !dict.get(v).into_iter().all(|p| !depends_on(&p, var, dict)),
        Slice(pats, slice) => {
            !pats.iter().all(|p| !depends_on(p, var, dict))
                || slice.as_ref().map_or(false, |v| {
                    v.as_str() == var
                        || !dict.get(v).into_iter().all(|p| !depends_on(&p, var, dict))
                })
        }
        _ => false,
    }
}

fn normalize(pat: Rc<UnifyingPat>, dict: &Dict) -> UnifyResult<UnifyingPat> {
    use UnifyingPat::*;

    let nf = substitute(&pat.unified(), dict)
        .map(|x| x.to_unify())
        .map_err(|_| CannotReduce(pat.clone()))?;
    let e = match &*nf {
        Eval(e) => e,
        _ => panic!("expected Evaluation, unexpected {}", pat.unified()),
    };

    match e {
        Evaluation::Add(x, y) => match (&**x, &**y) {
            (Num(m), Num(n)) => Ok(Num(m + n)),
            (Eval(_), Num(n)) => {
                if let Num(m) = normalize(x.clone(), dict)? {
                    Ok(Num(m + *n))
                } else {
                    Err(CannotReduce(x.clone()))
                }
            }
            (Num(m), Eval(_)) => {
                if let Num(n) = normalize(y.clone(), dict)? {
                    Ok(Num(*m + n))
                } else {
                    Err(CannotReduce(y.clone()))
                }
            }
            _ => Err(CannotReduce(pat)),
        },
        Evaluation::Sub(x, y) => match (&**x, &**y) {
            (Num(m), Num(n)) => Ok(Num(m - n)),
            (Eval(_), Num(n)) => {
                if let Num(m) = normalize(x.clone(), dict)? {
                    Ok(Num(m - *n))
                } else {
                    Err(CannotReduce(x.clone()))
                }
            }
            (Num(m), Eval(_)) => {
                if let Num(n) = normalize(y.clone(), dict)? {
                    Ok(Num(*m - n))
                } else {
                    Err(CannotReduce(y.clone()))
                }
            }
            _ => Err(CannotReduce(pat)),
        },
        Evaluation::Mul(x, y) => match (&**x, &**y) {
            (Num(m), Num(n)) => Ok(Num(m * n)),
            (Eval(_), Num(n)) => {
                if let Num(m) = normalize(x.clone(), dict)? {
                    Ok(Num(m * *n))
                } else {
                    Err(CannotReduce(x.clone()))
                }
            }
            (Num(m), Eval(_)) => {
                if let Num(n) = normalize(y.clone(), dict)? {
                    Ok(Num(*m * n))
                } else {
                    Err(CannotReduce(y.clone()))
                }
            }
            _ => Err(CannotReduce(pat)),
        },
        Evaluation::Div(x, y) => match (&**x, &**y) {
            (Num(m), Num(n)) => Ok(Num(m / n)),
            (Eval(_), Num(n)) => {
                if let Num(m) = normalize(x.clone(), dict)? {
                    Ok(Num(m / *n))
                } else {
                    Err(CannotReduce(x.clone()))
                }
            }
            (Num(m), Eval(_)) => {
                if let Num(n) = normalize(y.clone(), dict)? {
                    Ok(Num(*m / n))
                } else {
                    Err(CannotReduce(y.clone()))
                }
            }
            _ => Err(CannotReduce(pat)),
        },
        Evaluation::Append(x, y) => match (&**x, &**y) {
            (Str(m), Str(n)) => Ok(Str(m.clone() + n)),
            (Eval(_), Str(n)) => {
                if let Str(m) = normalize(x.clone(), dict)? {
                    Ok(Str(m + n))
                } else {
                    Err(CannotReduce(x.clone()))
                }
            }
            (Str(m), Eval(_)) => {
                if let Str(n) = normalize(y.clone(), dict)? {
                    Ok(Str(m.clone() + &n))
                } else {
                    Err(CannotReduce(y.clone()))
                }
            }
            _ => Err(CannotReduce(pat)),
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{arr, num, slice, string, var};

    #[test]
    fn test_unify() {
        // [?x, ?x..]
        let p = slice(vec![var("x")], "x");
        // [[?y, 2, 3], 1, ?z, 3]
        let q = arr(vec![
            arr(vec![var("y"), num(2), num(3)]),
            num(1),
            var("z"),
            num(3),
        ]);
        let mut dict = Dict::default();
        dict.unify(&p, &q).unwrap();
        assert_eq!(dict.subst(&p).unwrap(), dict.subst(&q).unwrap());

        // [?x, 1 + ?x]
        let p = arr(vec![var("x"), num(1) + var("x")]);
        // [3, 4]
        let q = arr(vec![num(3), num(4)]);
        let r = arr(vec![num(3), num(1) + num(3)]);
        let mut dict = Dict::default();
        dict.unify(&p, &q).unwrap();
        assert_eq!(dict.subst(&p).unwrap(), dict.subst(&r).unwrap());
    }

    #[test]
    fn test_bad_unify() {
        // [?x, ?x]
        let p = arr(vec![var("x"), var("x")]);
        // [?y, [a, ?y]]
        let q = arr(vec![var("y"), arr(vec![string("a"), var("y")])]);
        let mut dict = Dict::default();
        match dict.unify(&p, &q).err().unwrap() {
            CycleDependency(..) => {}
            _ => panic!("wrong answer"),
        }
    }
}

// [x, ...y]
// [1,[2,3,4,5,6,7]]
// [?u, ?v]
// ?u -> 1, ?v -> [2,3,4,5,6,7]

// [1,2,3,4,5,6,7]
// [?u, ...?v, ?r]
// ?u -> 1, ?v -> [2,3,4,5,6], ?r -> 6
