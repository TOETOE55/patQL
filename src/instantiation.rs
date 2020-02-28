use crate::ast::{Name, Pat, Query};
use crate::evaluation::Dict;
use crate::instantiation::InstantiateErr::{UnexpectedPattern, UnsolvedVariable};

#[derive(Clone, Debug)]
pub enum InstantiateErr {
    UnsolvedVariable(Name),
    UnexpectedPattern(Pat),
}

pub type InstantiateResult<T = ()> = Result<T, InstantiateErr>;

pub(crate) fn substitute(pat: &Pat, dict: &Dict) -> InstantiateResult<Pat> {
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

pub(crate) fn instantiate(q: &Query, dict: &Dict) -> InstantiateResult<Query> {
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
