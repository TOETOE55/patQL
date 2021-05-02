use crate::token::Span;
use lens_rs::*;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Number {
    pub span: Span,
    pub value: i64,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Str {
    pub span: Span,
    pub value: String,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Boolean {
    pub span: Span,
    pub value: bool,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Ident {
    pub span: Span,
    pub ident: String,
}

#[derive(Clone, PartialEq, Eq, Debug, Prism, Review)]
pub enum Pat {
    // 1, "abc", true...
    #[optic]
    Lit(LitPat),
    // x, y, z @ expr...
    #[optic]
    Ident(IdentPat),
    // _
    #[optic]
    Wild,
    // ..
    #[optic]
    Ellipse,
    // [x, 1, "abc"], [1, x @ .., "abc"]...
    #[optic]
    Arr(ArrPat),
    // x + 1...
    #[optic]
    Expr(ExprPat),
}

#[derive(Clone, PartialEq, Eq, Debug, Prism, Review)]
pub enum LitPat {
    #[optic]
    Str(crate::ast::Str),
    #[optic]
    Num(Number),
    #[optic]
    Bool(Boolean),
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct IdentPat {
    pub ident: Ident,
    pub sub_pat: Option<Box<Pat>>,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct ArrPat {
    pub items: Vec<Pat>,
}

#[derive(Clone, PartialEq, Eq, Debug, Prism, Review)]
pub enum ExprPat {
    #[optic]
    EQ((Box<Pat>, Box<Pat>)),

    #[optic]
    Add((Box<Pat>, Box<Pat>)),
    #[optic]
    Sub((Box<Pat>, Box<Pat>)),
    #[optic]
    Mul((Box<Pat>, Box<Pat>)),
    #[optic]
    Div((Box<Pat>, Box<Pat>)),
    #[optic]
    Rem((Box<Pat>, Box<Pat>)),

    #[optic]
    GT((Box<Pat>, Box<Pat>)),
    #[optic]
    GE((Box<Pat>, Box<Pat>)),
    #[optic]
    LT((Box<Pat>, Box<Pat>)),
    #[optic]
    LE((Box<Pat>, Box<Pat>)),

    #[optic]
    And((Box<Pat>, Box<Pat>)),
    #[optic]
    Or((Box<Pat>, Box<Pat>)),
    #[optic]
    Not((Box<Pat>, Box<Pat>)),

    #[optic]
    Append((Box<Pat>, Box<Pat>)),
    #[optic]
    Len((Box<Pat>, Box<Pat>)),
    #[optic]
    Index((Box<Pat>, Box<Pat>)),
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct RuleName {
    pub span: Span,
    pub ident: String,
}

// P 1
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Rule {
    pub name: RuleName,
    pub args: Pat,
}

#[derive(Clone, PartialEq, Eq, Debug, Prism, Review)]
pub enum Predicate {
    // ()
    #[optic]
    Unit,
    // P 1
    #[optic]
    Simple(Rule),
    // !P 1
    #[optic]
    Neg(Box<Predicate>),
    // any P 1, P 2,
    #[optic]
    All(Vec<Predicate>),
    // all P 1, P 2
    #[optic]
    Any(Vec<Predicate>),
    // for x y z. all P x, P y, P z
    #[optic]
    Forall(ForallPredicate),
    // 1 == 1
    #[optic]
    Builtin(BuiltinPredicate),
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct ForallPredicate {
    pub params: Vec<Ident>,
    pub predicate: Box<Predicate>,
}

#[derive(Clone, PartialEq, Eq, Debug, Prism, Review)]
pub enum BuiltinPredicate {
    // 1 == 2, [1, "abc"] == [1, "abc"]
    #[optic]
    EQ((Pat, Pat)),
    // 1 > 0
    #[optic]
    GT((Pat, Pat)),
    // 1 >= 0
    #[optic]
    GE((Pat, Pat)),
    #[optic]
    LT((Pat, Pat)),
    #[optic]
    LE((Pat, Pat)),

    #[optic]
    Println(Pat),
}

// P x <- any Q x, R x
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Decl {
    pub rule: Rule,
    pub pred: Predicate,
}

/*
in .pat
```
append [[], y, y] <- ()
append [[u, v @ ..], y, [u, z @ ..]] <-
    append [v, y, z]

main [] <- for x y. all
    append [x, y, [1,2,3,4,5,6,7]]
    println [x, y]

```

output:
```
[[], [1,2,3,4,5,6,7]]
[[1], [2,3,4,5,6,7]]
[[1,2], [3,4,5,6,7]]
[[1,2,3], [4,5,6,7]]
[[1,2,3,4], [5,6,7]]
[[1,2,3,4,5], [6,7]]
[[1,2,3,4,5,6], [7]]
[[1,2,3,4,5,6,7], []]
```
 */
