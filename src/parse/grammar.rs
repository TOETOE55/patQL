pub mod pat_phase {
    use super::*;
    use crate::ast::{arr, Pat};
    use psc::{lexeme, reg, wrap, ParseFn, ParseLogger, ParseState, Parser, ParserExt};

    pub fn tok_int() -> impl for<'a> Parser<ParseState<'a>, Target = Pat> {
        reg("-?[1-9]\\d*")
            .map(str::parse::<i64>)
            .map(Result::unwrap)
            .map(Pat::Num)
    }

    pub fn tok_str() -> impl for<'a> Parser<ParseState<'a>, Target = Pat> {
        reg("\"([^\"]|\\.)*\"")
            .map((|s: &str| &s[1..s.len() - 1]) as for<'a> fn(&'a str) -> &'a str)
            .map(str::to_owned)
            .map(Pat::Str)
    }

    pub fn tok_var() -> impl for<'a> Parser<ParseState<'a>, Target = Pat> {
        reg("\\?[0-9a-zA-Z\\-_$]+")
            .map((|s: &str| &s[1..]) as for<'a> fn(&'a str) -> &'a str)
            .map(str::to_owned)
            .map(Pat::Var)
    }

    pub fn arr_slice<'a>() -> impl Parser<ParseState<'a>, Target = Pat> {
        fn parse_arr(s: &mut ParseState, logger: &mut ParseLogger) -> Option<Pat> {
            lexeme('[').parse(s, logger)?;
            let mut prefix = (wrap(pat()) << lexeme(',')).many().parse(s, logger)?; // (pattern,)*
            let last = pat().parse(s, logger)?;
            lexeme(']').parse(s, logger)?;

            prefix.push(last);
            Some(arr(prefix))
        }

        fn parse_slice(s: &mut ParseState, logger: &mut ParseLogger) -> Option<Pat> {
            lexeme('[').parse(s, logger)?;
            let prefix = (wrap(pat()) << lexeme(',')).many().parse(s, logger)?; // (pattern,)*
            let last = (wrap("...") >> tok_var()).parse(s, logger)?;
            lexeme(']').parse(s, logger)?;

            let v = match last {
                Pat::Var(v) => v,
                p => panic!("unexpected {}", p),
            };

            Some(Pat::Arr(prefix, Some(v)))
        }

        let empty = wrap(lexeme('[')) >> psc::pure(|| arr(vec![])) << lexeme(']');

        wrap(empty) | ParseFn(parse_slice) | ParseFn(parse_arr)
    }

    pub fn pat<'a>() -> impl Parser<ParseState<'a>, Target = Pat> {
        fn parse_eval(stream: &mut ParseState, logger: &mut ParseLogger) -> Option<Pat> {
            evaluation::expr().parse(stream, logger)
        }

        wrap(arr_slice()) | ParseFn(parse_eval)
    }

    pub mod evaluation {
        use super::*;
        use crate::ast::Evaluation;
        use psc::pure;

        pub fn expr<'a>() -> impl Parser<ParseState<'a>, Target = Pat> {
            fn parse_expr(stream: &mut ParseState, logger: &mut ParseLogger) -> Option<Pat> {
                let e1 = mult().parse(stream, logger)?;
                let e2 = expr_().parse(stream, logger)?;
                Some(match e2 {
                    None => e1,
                    Some(f) => f(e1),
                })
            }
            ParseFn(parse_expr)
        }

        fn expr_<'a>() -> impl Parser<ParseState<'a>, Target = Option<Box<dyn FnOnce(Pat) -> Pat>>>
        {
            fn parse_plus(
                stream: &mut ParseState,
                logger: &mut ParseLogger,
            ) -> Option<Option<Box<dyn FnOnce(Pat) -> Pat>>> {
                lexeme('+').parse(stream, logger)?;
                let e1 = mult().parse(stream, logger)?;
                let e2 = expr_().parse(stream, logger)?;
                Some(Some(match e2 {
                    None => Box::new(move |e: Pat| e + e1) as Box<dyn FnOnce(_) -> _>,
                    Some(f) => Box::new(move |e| f(e + e1)),
                }))
            }

            fn parse_minu(
                stream: &mut ParseState,
                logger: &mut ParseLogger,
            ) -> Option<Option<Box<dyn FnOnce(Pat) -> Pat>>> {
                lexeme('-').parse(stream, logger)?;
                let e1 = mult().parse(stream, logger)?;
                let e2 = expr_().parse(stream, logger)?;
                Some(Some(match e2 {
                    None => Box::new(move |e: Pat| e - e1) as Box<dyn FnOnce(Pat) -> Pat>,
                    Some(f) => Box::new(move |e| f(e - e1)),
                }))
            }

            fn parse_append(
                stream: &mut ParseState,
                logger: &mut ParseLogger,
            ) -> Option<Option<Box<dyn FnOnce(Pat) -> Pat>>> {
                lexeme("<>").parse(stream, logger)?;
                let e1 = mult().parse(stream, logger)?;
                let e2 = expr_().parse(stream, logger)?;
                Some(Some(match e2 {
                    None => Box::new(move |e: Pat| e.append(e1)) as Box<dyn FnOnce(Pat) -> Pat>,
                    Some(f) => Box::new(move |e| f(e.append(e1))),
                }))
            }

            wrap(ParseFn(parse_plus)) | ParseFn(parse_minu) | ParseFn(parse_append) | pure(|| None)
        }

        fn mult<'a>() -> impl Parser<ParseState<'a>, Target = Pat> {
            fn parse_mul(stream: &mut ParseState, logger: &mut ParseLogger) -> Option<Pat> {
                let e1 = uexpr().parse(stream, logger)?;
                let e2 = mult_().parse(stream, logger)?;
                Some(match e2 {
                    None => e1,
                    Some(f) => f(e1),
                })
            }
            ParseFn(parse_mul)
        }

        fn mult_<'a>() -> impl Parser<ParseState<'a>, Target = Option<Box<dyn FnOnce(Pat) -> Pat>>>
        {
            fn parse_mul(
                stream: &mut ParseState,
                logger: &mut ParseLogger,
            ) -> Option<Option<Box<dyn FnOnce(Pat) -> Pat>>> {
                lexeme('*').parse(stream, logger)?;
                let e1 = uexpr().parse(stream, logger)?;
                let e2 = mult_().parse(stream, logger)?;
                Some(Some(match e2 {
                    None => Box::new(move |e: Pat| e * e1) as Box<dyn FnOnce(Pat) -> Pat>,
                    Some(f) => Box::new(move |e| f(e * e1)),
                }))
            }

            fn parse_div(
                stream: &mut ParseState,
                logger: &mut ParseLogger,
            ) -> Option<Option<Box<dyn FnOnce(Pat) -> Pat>>> {
                lexeme('/').parse(stream, logger)?;
                let e1 = uexpr().parse(stream, logger)?;
                let e2 = mult_().parse(stream, logger)?;
                Some(Some(match e2 {
                    None => Box::new(move |e: Pat| e / e1) as Box<dyn FnOnce(Pat) -> Pat>,
                    Some(f) => Box::new(move |e| f(e / e1)),
                }))
            }

            wrap(ParseFn(parse_mul)) | ParseFn(parse_div) | pure(|| None)
        }

        fn uexpr<'a>() -> impl Parser<ParseState<'a>, Target = Pat> {
            fn parse_paren(stream: &mut ParseState, logger: &mut ParseLogger) -> Option<Pat> {
                (wrap(lexeme('(')) >> expr() << ')').parse(stream, logger)
            }

            wrap(ParseFn(parse_paren))
                | tok_str()
                | tok_int()
                | tok_var()
        }
    }
}

/**
## left-recursion elimination
```BCNF
uop_term := '~' uop_term | '(' term ')' | pattern
biop_term := uop_term '&' conjoin_term | uop_term '|' biop_term | uop_term
term := disjoint_term
```
*/
pub mod term_phase {
    use crate::ast::{unit, Pat, Term};
    use crate::parse::grammar::pat_phase::pat;
    use psc::{lexeme, wrap, ParseFn, ParseLogger, ParseState, Parser, ParserExt};

    pub fn uop_term<'a>() -> impl Parser<ParseState<'a>, Target = Term> {
        fn parse_nega(s: &mut ParseState, logger: &mut ParseLogger) -> Option<Term> {
            (wrap(lexeme('~')) >> uop_term()).parse(s, logger)
        }

        fn parse_paren(s: &mut ParseState, logger: &mut ParseLogger) -> Option<Term> {
            (wrap(lexeme('(')) >> term() << lexeme(')')).parse(s, logger)
        }

        let unit = wrap(lexeme("unit")) >> psc::pure(unit);
        let nega = ParseFn(parse_nega);
        let paren = ParseFn(parse_paren);
        let pat = pat().map(Pat::q);
        unit | nega | paren | pat
    }

    pub fn biop_term<'a>() -> impl Parser<ParseState<'a>, Target = Term> {
        fn parse_disjoint(s: &mut ParseState, logger: &mut ParseLogger) -> Option<Term> {
            let nega = uop_term().parse(s, logger)?;
            lexeme('|').parse(s, logger)?;
            let disjoint = biop_term().parse(s, logger)?;
            Some(nega | disjoint)
        }

        fn parse_conjoin(s: &mut ParseState, logger: &mut ParseLogger) -> Option<Term> {
            let nega = uop_term().parse(s, logger)?;
            lexeme('&').parse(s, logger)?;
            let conjoin = biop_term().parse(s, logger)?;
            Some(nega & conjoin)
        }

        fn parse_nega(s: &mut ParseState, logger: &mut ParseLogger) -> Option<Term> {
            uop_term().parse(s, logger)
        }
        let disjoint = wrap(ParseFn(parse_disjoint));
        let conjoin = ParseFn(parse_conjoin);
        let nega = ParseFn(parse_nega);
        disjoint | conjoin | nega
    }

    pub fn term<'a>() -> impl Parser<ParseState<'a>, Target = Term> {
        biop_term()
    }
}
//
pub mod decl {
    use super::*;
    use crate::ast::Decl;
    use psc::{lexeme, wrap, ParseState, Parser, ParserExt};

    pub fn decl<'a>() -> impl Parser<ParseState<'a>, Target = Decl> {
        (wrap(pat_phase::pat()) << lexeme("=>")).map2(term_phase::term(), Decl::new)
    }

    pub fn decls<'a>() -> impl Parser<ParseState<'a>, Target = Vec<Decl>> {
        (wrap(decl()) << lexeme(';')).many().and_l(psc::EOF) //.snoc(decl().wrap() << lexem('.'))
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{arr, num, slice, string, var};
    use crate::parse::grammar::decl;
    use crate::parse::grammar::pat_phase::pat;
    use crate::parse::grammar::term_phase::term;
    use psc::{ParseLogger, ParseState, Parser};

    #[test]
    fn test_pat() {
        let mut src = ParseState::new("\"12  \\ 3\"");
        let mut logger = ParseLogger::default();
        let res = pat().parse(&mut src, &mut logger).unwrap();
        let p = string("12  \\ 3");
        assert_eq!(res, p);

        let mut src = ParseState::new("?-x_-8781d-sf");
        let mut logger = ParseLogger::default();
        let res = pat().parse(&mut src, &mut logger).unwrap();
        let p = var("-x_-8781d-sf");
        assert_eq!(res, p);

        let mut src = ParseState::new("-123193");
        let mut logger = ParseLogger::default();
        let res = pat().parse(&mut src, &mut logger).unwrap();
        let p = num(-123193);
        assert_eq!(res, p);

        let mut src = ParseState::new("[[1,2, ...?x], 1, \"\", ?x]");
        let mut logger = ParseLogger::default();
        let res = pat().parse(&mut src, &mut logger).unwrap();
        let p = arr(vec![
            slice(vec![num(1), num(2)], "x"),
            num(1),
            string(""),
            var("x"),
        ]);
        assert_eq!(res, p);

        let mut src = ParseState::new("[[1,2, ...?x], 1, \"\", ?x * (3 + 2)]");
        let mut logger = ParseLogger::default();
        let res = pat().parse(&mut src, &mut logger).unwrap();
        let p = arr(vec![
            slice(vec![num(1), num(2)], "x"),
            num(1),
            string(""),
            var("x") * (num(3) + num(2)),
        ]);
        assert_eq!(res, p);
    }

    #[test]
    fn test_term() {
        let mut src = ParseState::new("([?x, ...?x] & ?x) | \"123\"");
        let mut logger = ParseLogger::default();
        let res = term().parse(&mut src, &mut logger).unwrap();
        let t = (slice(vec![var("x")], "x").q() & var("x").q()) | string("123").q();
        assert_eq!(res, t);

        let mut src = ParseState::new("[?x, ...?x] & ?x | \"123\"");
        let mut logger = ParseLogger::default();
        let res = term().parse(&mut src, &mut logger).unwrap();
        let t = slice(vec![var("x")], "x").q() & (var("x").q() | string("123").q());
        assert_eq!(res, t);
    }

    #[test]
    fn test_decl() {
        let mut src = ParseState::new("?main => ([?x, ...?x] & ?x) | \"123\"");
        let mut logger = ParseLogger::default();
        let res = decl::decl().parse(&mut src, &mut logger).unwrap();
        let decl = var("main")
            .expand_to((slice(vec![var("x")], "x").q() & var("x").q()) | string("123").q());
        assert_eq!(res, decl);

        let mut src = ParseState::new(
            "?main => ([?x, ...?x] & ?x) | \"123\";\n?main => ([?x, ...?x] & ?x) | \"123\";",
        );
        let mut logger = ParseLogger::default();
        let res = decl::decls().parse(&mut src, &mut logger).unwrap();
        let decl = var("main")
            .expand_to((slice(vec![var("x")], "x").q() & var("x").q()) | string("123").q());
        assert_eq!(res, vec![decl.clone(), decl]);
    }
}
