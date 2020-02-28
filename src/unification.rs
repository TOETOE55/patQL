use crate::ast::{Name, Pat};
use crate::evaluation::Dict;
use crate::unification::UnifyErr::{CannotUnify, CycleDependency};

#[derive(Copy, Clone, Debug)]
pub enum UnifyingPat<'a> {
    Num(&'a i64),
    Str(&'a str),
    Var(&'a Name),
    Slice(&'a [Pat], Option<&'a Name>),
}

#[derive(Clone, Copy, Debug)]
pub enum UnifyErr<'a> {
    CannotUnify(UnifyingPat<'a>, UnifyingPat<'a>),
    CycleDependency(UnifyingPat<'a>, UnifyingPat<'a>),
}

pub type UnifyResult<'a, T = ()> = Result<T, UnifyErr<'a>>;

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

pub(crate) fn unify<'a>(
    p: UnifyingPat<'a>,
    q: UnifyingPat<'a>,
    dict: &mut Dict<'a>,
) -> UnifyResult<'a> {
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
