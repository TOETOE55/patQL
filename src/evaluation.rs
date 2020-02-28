use crate::ast::{Assertion, Pat, Query};
use crate::instantiation::{instantiate, substitute, InstantiateResult};
use crate::unification::{unify, UnifyResult, UnifyingPat};
use bumpalo::Bump;
use std::collections::HashMap;
use std::iter::once;
use std::slice::Iter;

#[derive(Clone, Debug, Default)]
pub struct Dict<'a> {
    dict: HashMap<&'a str, UnifyingPat<'a>>,
}

pub struct AssertionDriver {
    assertions: Vec<Assertion>,
    arena: Bump,
}

impl<'a> Dict<'a> {
    pub(crate) fn get(&self, var: &str) -> Option<UnifyingPat<'a>> {
        self.dict.get(var).copied()
    }

    pub(crate) fn insert(&mut self, var: &'a str, val: UnifyingPat<'a>) -> Option<UnifyingPat<'a>> {
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

impl AssertionDriver {
    pub fn new(assertions: Vec<Assertion>) -> Self {
        Self {
            assertions,
            arena: Default::default(),
        }
    }

    pub fn connect(&self) -> Iter<Assertion> {
        self.assertions.iter()
    }

    pub fn renamer(&self, assert: &Assertion) -> &Assertion {
        self.arena.alloc(assert.rename())
    }

    pub fn query<'a>(&'a self, q: &'a Query) -> impl Iterator<Item = Query> + 'a {
        run_query(q, self)
    }
}

pub(crate) fn run_query<'a>(
    q: &'a Query,
    driver: &'a AssertionDriver,
) -> impl Iterator<Item = Query> + 'a {
    qeval(q, driver, once(Dict::default())).flat_map(move |dict| dict.inst(q))
}

pub(crate) fn qeval<'a, I>(
    q: &'a Query,
    driver: &'a AssertionDriver,
    dicts: I,
) -> Box<dyn Iterator<Item = Dict<'a>> + 'a>
where
    I: Iterator<Item = Dict<'a>> + 'a,
{
    match q {
        Query::True => Box::new(dicts),
        Query::Simple(p) => Box::new(apply_asserts(p, driver, dicts)),
        Query::Conjoin(p, q) => qeval(p, driver, qeval(q, driver, dicts)),
        Query::Disjoint(p, q) => Box::new(dicts.flat_map(move |dict| {
            qeval(p, driver, once(dict.clone())).chain(qeval(q, driver, once(dict)))
        })),
        Query::Negative(p) => Box::new(dicts.flat_map(move |dict| {
            let v = qeval(p, driver, once(dict.clone())).collect::<Vec<_>>();
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

fn apply_asserts<'a, I>(
    pat: &'a Pat,
    driver: &'a AssertionDriver,
    dicts: I,
) -> impl Iterator<Item = Dict<'a>> + 'a
where
    I: Iterator<Item = Dict<'a>> + 'a,
{
    dicts.flat_map(move |dict| {
        driver.connect().flat_map(move |asst| {
            let mut dict = dict.clone();
            let renamed = driver.renamer(asst);
            if let Ok(()) = dict.unify(pat, &renamed.conclusion) {
                qeval(&renamed.body, driver, once(dict))
            } else {
                Box::new(None.into_iter())
            }
        })
    })
}
