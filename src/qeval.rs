use crate::ast::{Assertion, Name, Pat, Query};
use crate::unify::{instantiate, substitute, unify, UnifyResult};
use std::collections::{HashMap, VecDeque};
use std::rc::Rc;

#[derive(Debug, Default, Clone)]
pub struct Dict {
    pub dict: HashMap<String, Rc<Pat>>,
}

impl Dict {
    pub fn unify(&mut self, p: Rc<Pat>, q: Rc<Pat>) -> UnifyResult<()> {
        unify(p, q, self)
    }

    pub fn subst(&self, p: &Pat) -> UnifyResult<Pat> {
        substitute(p, &self)
    }

    pub fn instantiate(&self, q: &Query) -> UnifyResult<Query> {
        instantiate(q, self)
    }

    pub fn get(&self, name: &Name) -> Option<Rc<Pat>> {
        self.dict.get(name).cloned()
    }

    pub fn remove(&mut self, name: &Name) -> Option<Rc<Pat>> {
        self.dict.remove(name)
    }

    pub fn insert(&mut self, name: String, pat: Rc<Pat>) -> Option<Rc<Pat>> {
        self.dict.insert(name, pat)
    }
}

#[derive(Clone, Debug)]
pub struct DictStream {
    buf: VecDeque<Dict>,
}

impl Default for DictStream {
    fn default() -> Self {
        Self {
            buf: vec![Dict::default()].into(),
        }
    }
}

impl Iterator for DictStream {
    type Item = Dict;

    fn next(&mut self) -> Option<Self::Item> {
        self.buf.pop_front()
    }
}

impl DictStream {
    pub fn empty() -> Self {
        Self {
            buf: Default::default(),
        }
    }

    pub fn single(dict: Dict) -> Self {
        Self {
            buf: vec![dict].into(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.buf.is_empty()
    }

    pub fn write(&mut self, src: Dict) {
        self.buf.push_back(src);
    }

    pub fn writes(&mut self, src: &mut DictStream) {
        self.buf.append(&mut src.buf);
    }

    pub fn read(&mut self) -> Option<Dict> {
        self.next()
    }

    pub fn reads(&mut self, dest: &mut DictStream) {
        std::mem::swap(&mut self.buf, &mut dest.buf)
    }
}

pub fn run_query<'a>(q: &Query, db: &[Assertion]) -> Vec<Query> {
    let mut inputs = DictStream::default();
    let mut outputs = DictStream::empty();
    query(q, db, &mut inputs, &mut outputs);

    outputs.fold(vec![], |mut qs, dict| {
        if let Ok(res) = dict.instantiate(q) {
            qs.push(res)
        }
        qs
    })
}

pub fn query(q: &Query, assts: &[Assertion], inputs: &mut DictStream, outputs: &mut DictStream) {
    match q {
        Query::True => outputs.writes(inputs),
        Query::Simple(p) => apply_asst(p, assts, inputs, outputs),
        Query::Conjoin(p, q) => {
            let mut tmp_s = DictStream::empty();
            query(p, assts, inputs, &mut tmp_s);
            query(q, assts, &mut tmp_s, outputs);
        }
        Query::Disjoint(p, q) => {
            let mut inputs_cp = inputs.clone();
            query(p, assts, inputs, outputs);
            query(q, assts, &mut inputs_cp, outputs);
        }
        Query::Negative(q) => {
            for dict in inputs {
                let mut tmp = DictStream::empty();
                query(q, assts, &mut DictStream::single(dict.clone()), &mut tmp);
                if tmp.is_empty() {
                    outputs.write(dict)
                }
            }
        }
    }
}

fn apply_asst(pat: &Pat, assts: &[Assertion], inputs: &mut DictStream, outputs: &mut DictStream) {
    for asst in assts {
        let renamed = asst.rename();
        for dict in inputs.buf.iter() {
            let mut dict = dict.clone();
            if let Ok(()) = dict.unify(Rc::new(pat.clone()), Rc::new(renamed.conclusion.clone())) {
                query(
                    &renamed.body,
                    assts,
                    &mut DictStream::single(dict.clone()),
                    outputs,
                );
            }
        }
    }
}
