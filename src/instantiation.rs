use crate::ast::{Name, Pat, Term};
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

pub(crate) fn instantiate(q: &Term, dict: &Dict) -> InstantiateResult<Term> {
    Ok(match q {
        Term::Unit => Term::Unit,
        Term::Simple(p) => Term::Simple(substitute(p, dict)?),
        Term::Conjoin(p, q) => Term::Conjoin(
            Box::new(instantiate(p, dict)?),
            Box::new(instantiate(q, dict)?),
        ),
        Term::Disjoint(p, q) => Term::Disjoint(
            Box::new(instantiate(p, dict)?),
            Box::new(instantiate(q, dict)?),
        ),
        Term::Negative(q) => instantiate(q, dict)?,
        Term::Filter(d, f, p) => Term::Filter(d.clone(), *f, substitute(p, dict)?),
    })
}
