use crate::object::{IdentPatObj, PatObj};
use std::collections::{HashMap, VecDeque};
use std::rc::Rc;
use thiserror::Error;

pub type Dict = HashMap<u128, Rc<PatObj>>;

#[derive(Clone, Debug, Eq, PartialEq, Error)]
pub enum UnifyErr {
    #[error("cannot unify {0:?} with {1:?}")]
    CannotUnify(Rc<PatObj>, Rc<PatObj>),
    #[error("cycle dependency {0:?} and {1:?}")]
    CycleDependency(Rc<PatObj>, Rc<PatObj>),
    #[error("can't reduce {0:?}")]
    CannotReduce(Rc<PatObj>),
}

pub type Result<T> = std::result::Result<T, UnifyErr>;

pub fn unify(p: &PatObj, q: &PatObj, dict: Dict) -> Result<Dict> {
    use PatObj::*;
    match (p, q) {
        (Wild, _) | (_, Wild) => Ok(dict),
        (Lit(l1), Lit(l2)) if l1 == l2 => Ok(dict),
        (Ident(ident_pat), _) => {}
        _ => vec![],
    }
    vec![]
}

fn solve(id_pat: &IdentPatObj, val: &PatObj, dict: Dict) -> Vec<Result<Dict>> {}
