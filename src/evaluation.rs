use crate::ast::{Decl, Name, Pat, Term};
use crate::instantiation::{instantiate, substitute, InstantiateResult};
use crate::unification::{unify, UnifyResult, UnifyingPat};
use bumpalo::Bump;
use std::collections::HashMap;
use std::iter::once;
use std::rc::Rc;
use std::slice::Iter;

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct Dict {
    dict: HashMap<Rc<Name>, Rc<UnifyingPat>>,
}

pub struct Driver {
    decls: Vec<Decl>,
    arena: Bump,
}

impl Dict {
    pub(crate) fn get(&self, var: &Name) -> Option<Rc<UnifyingPat>> {
        self.dict.get(var).cloned()
    }

    pub(crate) fn insert(
        &mut self,
        var: Rc<Name>,
        val: Rc<UnifyingPat>,
    ) -> Option<Rc<UnifyingPat>> {
        self.dict.insert(var, val)
    }

    pub fn unify(&mut self, p: &Pat, q: &Pat) -> UnifyResult {
        unify(p.to_unify(), q.to_unify(), self)
    }

    pub fn subst(&self, pat: &Pat) -> InstantiateResult<Pat> {
        substitute(pat, self)
    }

    pub fn inst(&self, q: &Term) -> InstantiateResult<Term> {
        instantiate(q, self)
    }
}

impl Driver {
    pub fn new(decls: Vec<Decl>) -> Self {
        Self {
            decls,
            arena: Default::default(),
        }
    }

    pub fn connect(&self) -> Iter<Decl> {
        self.decls.iter()
    }

    pub fn renamer(&self, assert: &Decl) -> &Decl {
        self.arena.alloc(assert.rename())
    }

    pub fn query<'a>(&'a self, q: &'a Term) -> impl Iterator<Item = Term> + 'a {
        run_query(q, self)
    }
}

pub(crate) fn run_query<'a>(q: &'a Term, driver: &'a Driver) -> impl Iterator<Item = Term> + 'a {
    qeval(q, driver, once(Dict::default())).flat_map(move |dict| dict.inst(q))
}

pub(crate) fn qeval<'a, I>(
    q: &'a Term,
    driver: &'a Driver,
    dicts: I,
) -> Box<dyn Iterator<Item = Dict> + 'a>
where
    I: Iterator<Item = Dict> + 'a,
{
    match q {
        Term::Unit => Box::new(dicts),
        Term::Simple(p) => Box::new(apply_asserts(p, driver, dicts)),
        Term::Conjoin(p, q) => qeval(q, driver, qeval(p, driver, dicts)),
        Term::Disjoint(p, q) => Box::new(dicts.flat_map(move |dict| {
            qeval(p, driver, once(dict.clone())).chain(qeval(q, driver, once(dict)))
        })),
        Term::Negative(p) => Box::new(dicts.flat_map(move |dict| {
            let v = qeval(p, driver, once(dict.clone())).collect::<Vec<_>>();
            if v.is_empty() {
                Some(dict)
            } else {
                None
            }
        })),
        Term::Filter(_, f, p) => Box::new(dicts.filter(move |dict| {
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
    driver: &'a Driver,
    dicts: I,
) -> impl Iterator<Item = Dict> + 'a
where
    I: Iterator<Item = Dict> + 'a,
{
    dicts.flat_map(move |dict| {
        driver.connect().flat_map(move |decl| {
            let mut dict = dict.clone();
            let renamed = driver.renamer(decl);
            if let Ok(()) = dict.unify(pat, &renamed.pat) {
                qeval(&renamed.expanded, driver, once(dict))
            } else {
                Box::new(None.into_iter())
            }
        })
    })
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::ast::*;

    #[test]
    fn test_eval1() {
        let rule1 = arr(vec![string("append"), arr(vec![]), var("y"), var("y")]).expand_to(unit());
        let rule2 = arr(vec![
            string("append"),
            slice(vec![var("u")], "v"),
            var("y"),
            slice(vec![var("u")], "z"),
        ])
        .expand_to(arr(vec![string("append"), var("v"), var("y"), var("z")]).q());

        let db = Driver::new(vec![rule2, rule1]);
        let qry1 = arr(vec![
            string("append"),
            var("x"),
            var("y"),
            arr(vec![num(1), num(2), num(3), num(4)]),
        ])
        .q();

        let result = db.query(&qry1).collect::<Vec<_>>();

        let ans1 = arr(vec![
            string("append"),
            arr(vec![]),
            arr(vec![num(1), num(2), num(3), num(4)]),
            arr(vec![num(1), num(2), num(3), num(4)]),
        ])
        .q();
        let ans2 = arr(vec![
            string("append"),
            arr(vec![num(1)]),
            arr(vec![num(2), num(3), num(4)]),
            arr(vec![num(1), num(2), num(3), num(4)]),
        ])
        .q();
        let ans3 = arr(vec![
            string("append"),
            arr(vec![num(1), num(2)]),
            arr(vec![num(3), num(4)]),
            arr(vec![num(1), num(2), num(3), num(4)]),
        ])
        .q();
        let ans4 = arr(vec![
            string("append"),
            arr(vec![num(1), num(2), num(3)]),
            arr(vec![num(4)]),
            arr(vec![num(1), num(2), num(3), num(4)]),
        ])
        .q();
        let ans5 = arr(vec![
            string("append"),
            arr(vec![num(1), num(2), num(3), num(4)]),
            arr(vec![]),
            arr(vec![num(1), num(2), num(3), num(4)]),
        ])
        .q();

        assert!(!result
            .iter()
            .all(|x| format!("{}", x) != format!("{}", ans1)));
        assert!(!result
            .iter()
            .all(|x| format!("{}", x) != format!("{}", ans2)));
        assert!(!result
            .iter()
            .all(|x| format!("{}", x) != format!("{}", ans3)));
        assert!(!result
            .iter()
            .all(|x| format!("{}", x) != format!("{}", ans4)));
        assert!(!result
            .iter()
            .all(|x| format!("{}", x) != format!("{}", ans5)));
    }

    #[test]
    fn test_eval2() {
        let rule1 = arr(vec![string("append"), arr(vec![]), var("y"), var("y")]).expand_to(unit());
        let rule2 = arr(vec![
            string("append"),
            slice(vec![var("u")], "v"),
            var("y"),
            slice(vec![var("u")], "z"),
        ])
        .expand_to(arr(vec![string("append"), var("v"), var("y"), var("z")]).q());

        let db = Driver::new(vec![rule2, rule1]);
        let qry1 = arr(vec![
            string("append"),
            arr(vec![num(1), num(2)]),
            arr(vec![num(3), num(4)]),
            var("x"),
        ])
        .q();

        let result = db.query(&qry1).collect::<Vec<_>>();

        let ans = arr(vec![
            string("append"),
            arr(vec![num(1), num(2)]),
            arr(vec![num(3), num(4)]),
            arr(vec![num(1), num(2), num(3), num(4)]),
        ])
        .q();

        assert!(!result
            .iter()
            .all(|x| format!("{}", x) != format!("{}", ans)));
    }

    #[test]
    fn test_eval3() {
        let apple = arr(vec![string("apple"), num(3)]).datum();
        let banana = arr(vec![string("banana"), num(4)]).datum();
        let pear = arr(vec![string("pear"), num(5)]).datum();

        let drive = Driver::new(vec![apple, banana, pear]);

        fn greater(p: &Pat) -> bool {
            match p {
                Pat::Num(n) => *n > 3,
                _ => false,
            }
        }

        let q = arr(vec![var("fruits"), var("amount")]).q() & var("amount").filter("(>3)", greater);

        let result = drive.query(&q).collect::<Vec<_>>();

        let ans1 = arr(vec![string("banana"), num(4)]).q() & num(4).filter("(>3)", greater);
        let ans2 = arr(vec![string("pear"), num(5)]).q() & num(5).filter("(>3)", greater);

        assert!(!result
            .iter()
            .all(|x| format!("{}", x) != format!("{}", ans1)));
        assert!(!result
            .iter()
            .all(|x| format!("{}", x) != format!("{}", ans2)));
    }
}
