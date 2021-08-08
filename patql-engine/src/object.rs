use std::rc::Rc;

#[derive(Clone, PartialEq, Eq, Debug, Prism, Review)]
pub enum PatObj {
    // 1, "abc", true...
    #[optic]
    Lit(LitPatObj),
    // x, y, z @ expr...
    #[optic]
    Ident(IdentPatObj),
    // _
    #[optic]
    Wild,
    // ..
    #[optic]
    Ellipse,
    // [x, 1, "abc"], [1, x @ .., "abc"]...
    #[optic]
    Arr(ArrPatObj),
    // {x + 1}...
    #[optic]
    Expr(ExprPatObj),
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct LitPatObj(LitObj);

// x @ ..
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct IdentPatObj {
    pub var: Var,
    pub sub_pat: Option<Rc<PatObj>>,
}

#[derive(Clone, PartialEq, Eq, Debug, Lens)]
pub struct ArrPatObj {
    #[optic]
    pub items: Vec<Rc<PatObj>>,
}

#[derive(Clone, PartialEq, Eq, Debug, Prism, Review)]
pub enum ExprPatObj {
    #[optic]
    Pure(Rc<PatObj>),

    #[optic]
    EQ((Rc<PatObj>, Rc<PatObj>)),
    #[optic]
    NE((Rc<PatObj>, Rc<PatObj>)),

    #[optic]
    Neg(Rc<PatObj>),
    #[optic]
    Add((Rc<PatObj>, Rc<PatObj>)),
    #[optic]
    Sub((Rc<PatObj>, Rc<PatObj>)),
    #[optic]
    Mul((Rc<PatObj>, Rc<PatObj>)),
    #[optic]
    Div((Rc<PatObj>, Rc<PatObj>)),
    #[optic]
    Rem((Rc<PatObj>, Rc<PatObj>)),

    #[optic]
    GT((Rc<PatObj>, Rc<PatObj>)),
    #[optic]
    GE((Rc<PatObj>, Rc<PatObj>)),
    #[optic]
    LT((Rc<PatObj>, Rc<PatObj>)),
    #[optic]
    LE((Rc<PatObj>, Rc<PatObj>)),

    #[optic]
    And((Rc<PatObj>, Rc<PatObj>)),
    #[optic]
    Or((Rc<PatObj>, Rc<PatObj>)),
    #[optic]
    Not(Rc<PatObj>),

    #[optic]
    Append((Rc<PatObj>, Rc<PatObj>)),
    #[optic]
    Len(Rc<PatObj>),
    #[optic]
    Index((Rc<PatObj>, Rc<PatObj>)),
}

#[derive(Clone, Eq, PartialEq, Debug, Prism, Review)]
pub enum LitObj {
    #[optic]
    Int(i64),
    #[optic]
    Bool(bool),
    #[optic]
    Str(String),
}

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq)]
struct Var {
    pub id: u128,
    pub name: String,
}
