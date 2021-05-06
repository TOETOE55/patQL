use crate::parse::{Parse, ParseStream, Result};
use crate::token::group::{Brace, Bracket, Paren};
use crate::token::helper::{LBrace, LBracket, LParen, Punctuated, WhiteSpace};
use crate::token::keyword::{All, Any, For};
use crate::token::lit::Lit;

use crate::token::layout::Align;
use crate::token::punctuate::{Add, At, Bang, Comma, Dot, Dot2, LArrow, Punct, Sub, Underscore};
use crate::token::{Ident, TokenTree};
use lens_rs::*;
use std::result::Result::Ok;

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
    // {x + 1}...
    #[optic]
    Expr(ExprPat),
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct LitPat(Lit);

// x @ ..
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
    Pure(Box<Pat>),

    #[optic]
    EQ((Box<Pat>, Box<Pat>)),
    #[optic]
    NE((Box<Pat>, Box<Pat>)),

    #[optic]
    Neg(Box<Pat>),
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
    Not(Box<Pat>),

    #[optic]
    Append((Box<Pat>, Box<Pat>)),
    #[optic]
    Len(Box<Pat>),
    #[optic]
    Index((Box<Pat>, Box<Pat>)),
}

// P 1
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Rule {
    pub name: Ident,
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
    // any P 1, P 2
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
    #[optic]
    NE((Pat, Pat)),
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

// P x <- any (all R x), Q x
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Decl {
    pub rule: Rule,
    pub pred: Predicate,
}

// P x <- any Q x, R x
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct File {
    pub decls: Vec<Decl>,
}

/*
in .pq
```
append [[], y, y] <- ()
append [[u, v @ ..], y, [u, z @ ..]] <- append [v, y, z]

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

impl Parse<'_> for Pat {
    fn parse(stream: &mut ParseStream<'_>) -> Result<Self> {
        Ok(if stream.peek::<LitPat>() {
            Pat::Lit(stream.parse()?)
        } else if stream.peek::<IdentPat>() {
            Pat::Ident(stream.parse()?)
        } else if stream.peek::<Dot2>() {
            stream.parse::<Dot2>();
            Pat::Ellipse
        } else if stream.peek::<Underscore>() {
            stream.parse::<Underscore>();
            Pat::Wild
        } else if stream.peek::<LBrace>() {
            Pat::Expr(stream.parse()?)
        } else {
            Pat::Arr(stream.parse::<ArrPat>()?)
        })
    }
}

impl Parse<'_> for LitPat {
    fn parse(stream: &mut ParseStream<'_>) -> Result<Self> {
        Ok(LitPat(stream.parse::<Lit>()?))
    }
}

impl Parse<'_> for IdentPat {
    fn parse(stream: &mut ParseStream<'_>) -> Result<Self> {
        let ident = stream.parse()?;
        let sub_pat = if stream.peek::<At>() {
            stream.parse::<At>()?;
            Some(Box::new(stream.parse()?))
        } else {
            None
        };

        Ok(Self { ident, sub_pat })
    }
}

impl Parse<'_> for ArrPat {
    fn parse(stream: &mut ParseStream<'_>) -> Result<Self> {
        let mut bracket = stream.parse::<Bracket>()?;
        let items = bracket.sub_stream.parse::<Punctuated<_, Comma>>()?.items;
        if let Some((span, word)) = bracket.sub_stream.next() {
            return Err(anyhow::anyhow!(
                "expected `]` found `{}` in {:?}",
                word,
                span
            ));
        }

        Ok(Self { items })
    }
}

pub fn parse_unary(stream: &mut ParseStream) -> Result<Pat> {
    let pat = if stream.peek::<Sub>() {
        stream.parse::<Sub>()?;
        Pat::Expr(ExprPat::Neg(Box::new(stream.parse()?)))
    } else if stream.peek::<Bang>() {
        stream.parse::<Bang>()?;
        Pat::Expr(ExprPat::Not(Box::new(stream.parse()?)))
    } else if stream.peek::<Add>() {
        stream.parse::<Add>()?;
        stream.parse()?
    } else if stream.peek::<IdentPat>() {
        let id = stream.parse::<IdentPat>()?;
        if &id.ident.name == "len" && id.sub_pat.is_none() {
            Pat::Expr(ExprPat::Len(Box::new(stream.parse()?)))
        } else {
            Pat::Ident(id)
        }
    } else if stream.peek::<Lit>() {
        Pat::Lit(stream.parse::<LitPat>()?)
    } else if stream.peek::<Dot2>() {
        stream.parse::<Dot2>()?;
        Pat::Ellipse
    } else if stream.peek::<Underscore>() {
        stream.parse::<Underscore>()?;
        Pat::Wild
    } else if stream.peek::<ArrPat>() {
        Pat::Arr(stream.parse::<ArrPat>()?)
    } else {
        let mut sub_stream = stream.parse::<Paren>()?.sub_stream;
        let inner_pat = stream.parse::<Pat>()?;
        if let Some((span, word)) = sub_stream.next() {
            return Err(anyhow::anyhow!(
                "expected terminated found {} in {:?}",
                word,
                span
            ));
        } else {
            inner_pat
        }
    };

    let pat = if stream.peek::<Bracket>() {
        let mut sub_stream = stream.parse::<Bracket>()?.sub_stream;
        let inner_pat = stream.parse::<Pat>()?;
        if let Some((span, word)) = sub_stream.next() {
            return Err(anyhow::anyhow!(
                "expected terminated found {} in {:?}",
                word,
                span
            ));
        } else {
            Pat::Expr(ExprPat::Index((Box::new(pat), Box::new(inner_pat))))
        }
    } else {
        pat
    };

    Ok(pat)
}

fn parse_ambiguous(stream: &mut ParseStream, lhs: Pat) -> Result<ExprPat> {
    let bi_op = if let Ok(op) = stream.parse::<Punct>() {
        op
    } else {
        return Ok(ExprPat::Pure(Box::new(lhs)));
    };
    let rhs = stream.parse::<Pat>()?;

    let cons = match bi_op {
        Punct::EQ(_) => ExprPat::EQ,
        Punct::NE(_) => ExprPat::NE,
        Punct::GT(_) => ExprPat::GT,
        Punct::GE(_) => ExprPat::GE,
        Punct::LT(_) => ExprPat::LT,
        Punct::LE(_) => ExprPat::LE,
        Punct::Add(_) => ExprPat::Add,
        Punct::Sub(_) => ExprPat::Sub,
        Punct::Mul(_) => ExprPat::Mul,
        Punct::Div(_) => ExprPat::Div,
        Punct::Rem(_) => ExprPat::Rem,
        Punct::And(_) => ExprPat::And,
        Punct::Or(_) => ExprPat::Or,
        Punct::Append(_) => ExprPat::Append,
        p => {
            return Err(anyhow::anyhow!(
                "expected binary op found {:?} in {:?}",
                p,
                p.span()
            ))
        }
    };

    Ok(cons((Box::new(lhs), Box::new(rhs))))
}

impl Parse<'_> for ExprPat {
    fn parse(stream: &mut ParseStream<'_>) -> Result<Self> {
        let mut sub_stream = stream.parse::<Brace>()?.sub_stream;
        let lhs = parse_unary(&mut sub_stream)?;
        let expr = parse_ambiguous(&mut sub_stream, lhs)?;
        if let Some((span, word)) = sub_stream.next() {
            return Err(anyhow::anyhow!(
                "expected terminated found {} in {:?}",
                word,
                span
            ));
        }
        Ok(expr)
    }
}

impl Parse<'_> for Rule {
    fn parse(stream: &mut ParseStream<'_>) -> Result<Self> {
        Ok(Self {
            name: stream.parse()?,
            args: stream.parse()?,
        })
    }
}

fn parse_align_or_punctuated(stream: &mut ParseStream<'_>) -> Result<Vec<Predicate>> {
    Ok(if stream.peek2::<Predicate, Comma>() {
        stream.parse::<Punctuated<Predicate, Comma>>()?.items
    } else {
        let mut ps = vec![];
        for mut sub_stream in stream.parse::<Align>()?.sub_streams {
            ps.push(sub_stream.parse::<Predicate>()?);
            if let Ok(tok) = sub_stream.parse::<TokenTree>() {
                return Err(anyhow::anyhow!(
                    "expected `\n` found {:?} in {:?}",
                    tok,
                    tok.span()
                ));
            }
        }
        ps
    })
}

impl Parse<'_> for Predicate {
    fn parse(stream: &mut ParseStream<'_>) -> Result<Self> {
        let p = if stream.peek::<LParen>() {
            let mut sub_stream = stream.parse::<Paren>()?.sub_stream;
            if sub_stream.peek::<TokenTree>() {
                sub_stream.parse()?
            } else {
                Predicate::Unit
            }
        } else if stream.peek::<Bang>() {
            stream.parse::<Bang>()?;
            Predicate::Neg(Box::new(stream.parse()?))
        } else if stream.peek::<Any>() {
            stream.parse::<Any>()?;
            Predicate::Any(parse_align_or_punctuated(stream)?)
        } else if stream.peek::<All>() {
            stream.parse::<All>()?;
            Predicate::All(parse_align_or_punctuated(stream)?)
        } else if stream.peek::<For>() {
            Predicate::Forall(stream.parse::<ForallPredicate>()?)
        } else if stream.peek::<BuiltinPredicate>() {
            Predicate::Builtin(stream.parse()?)
        } else {
            Predicate::Simple(stream.parse()?)
        };

        Ok(p)
    }
}

impl Parse<'_> for ForallPredicate {
    fn parse(stream: &mut ParseStream<'_>) -> Result<Self> {
        stream.parse::<For>()?;
        let mut params = vec![];
        while stream.peek::<Ident>() {
            let ident = stream.parse().unwrap();
            params.push(ident);
        }
        stream.parse::<Dot>()?;

        Ok(Self {
            params,
            predicate: Box::new(stream.parse()?),
        })
    }
}

impl Parse<'_> for BuiltinPredicate {
    fn parse(stream: &mut ParseStream<'_>) -> Result<Self> {
        if let Ok(id) = stream.lookahead::<Ident>() {
            if id.name == "println" {
                stream.parse::<Ident>()?;
                return Ok(BuiltinPredicate::Println(stream.parse()?));
            }
        }

        let lhs = stream.parse()?;
        let bi_op = stream.parse()?;
        let rhs = stream.parse()?;

        let cons = match bi_op {
            Punct::EQ(_) => BuiltinPredicate::EQ,
            Punct::NE(_) => BuiltinPredicate::NE,
            Punct::GT(_) => BuiltinPredicate::GT,
            Punct::GE(_) => BuiltinPredicate::GE,
            Punct::LT(_) => BuiltinPredicate::LT,
            Punct::LE(_) => BuiltinPredicate::LE,
            p => {
                return Err(anyhow::anyhow!(
                    "expected binary op found {:?} in {:?}",
                    p,
                    p.span()
                ))
            }
        };
        Ok(cons((lhs, rhs)))
    }
}

impl Parse<'_> for Decl {
    fn parse(stream: &mut ParseStream<'_>) -> Result<Self> {
        let rule = stream.parse()?;
        stream.parse::<LArrow>()?;
        let pred = stream.parse()?;
        Ok(Self { rule, pred })
    }
}

impl Parse<'_> for File {
    fn parse(stream: &mut ParseStream<'_>) -> Result<Self> {
        let align = stream.parse::<Align>()?;

        let mut decls = vec![];
        for mut sub_stream in align.sub_streams {
            decls.push(sub_stream.parse()?);
            if let Ok(tok) = sub_stream.parse::<TokenTree>() {
                return Err(anyhow::anyhow!("expected decl found {:?}", tok));
            }
        }

        if let Ok(tok) = stream.parse::<TokenTree>() {
            return Err(anyhow::anyhow!("expected terminated found {:?}", tok));
        }

        Ok(Self { decls })
    }
}
