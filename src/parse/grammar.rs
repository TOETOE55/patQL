use psc::covert::IntoParser;
use psc::{reg, ParseFn, ParseMsg, ParseState, Parser};

fn lexem<'a, A>(
    p: impl IntoParser<ParseState<'a>, Target = A>,
) -> impl Parser<ParseState<'a>, Target = A> {
    let blank = psc::char('\n').or(' ').or('\t').or('\r');
    blank.clone().many_().wrap() >> p << blank.many_()
}

pub mod pat_phase {
    use super::*;
    use crate::ast::{arr, Pat};

    pub fn tok_int<'a>() -> impl Parser<ParseState<'a>, Target = Pat> {
        reg("-?[1-9]\\d*")
            .map(str::parse::<i64>)
            .map(Result::unwrap)
            .map(Pat::Num)
    }

    pub fn tok_str<'a>() -> impl Parser<ParseState<'a>, Target = Pat> {
        reg("\"([^\"]|\\.)*\"")
            .map(|s: &str| &s[1..s.len() - 1])
            .map(str::to_owned)
            .map(Pat::Str)
    }

    pub fn tok_var<'a>() -> impl Parser<ParseState<'a>, Target = Pat> {
        reg("\\?[0-9a-zA-Z\\-_$]+")
            .map(|s: &str| &s[1..])
            .map(str::to_owned)
            .map(Pat::Var)
    }

    pub fn arr_slice<'a>() -> impl Parser<ParseState<'a>, Target = Pat> {
        fn parse_arr(s: &mut ParseState) -> Result<Pat, ParseMsg> {
            lexem('[').parse(s)?;
            let mut prefix = (pat().wrap() << lexem(',')).many().parse(s)?; // (pattern,)*
            let last = pat().parse(s)?;
            lexem(']').parse(s)?;

            prefix.push(last);
            Ok(arr(prefix))
        }

        fn parse_slice(s: &mut ParseState) -> Result<Pat, ParseMsg> {
            lexem('[').parse(s)?;
            let prefix = (pat().wrap() << lexem(',')).many().parse(s)?; // (pattern,)*
            let last = (psc::strg("...").wrap() >> tok_var()).parse(s)?;
            lexem(']').parse(s)?;

            let v = match last {
                Pat::Var(v) => v,
                p => panic!("unexpected {}", p),
            };

            Ok(Pat::Arr(prefix, Some(v)))
        }

        let empty = lexem('[').wrap() >> psc::pure(|| arr(vec![])) << lexem(']');
        empty | ParseFn(parse_slice) | ParseFn(parse_arr)
    }

    pub fn pat<'a>() -> impl Parser<ParseState<'a>, Target = Pat> {
        tok_str().wrap() | tok_int() | tok_var() | arr_slice()
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
    use super::*;
    use crate::ast::{unit, Pat, Term};
    use crate::parse::grammar::pat_phase::pat;

    pub fn uop_term<'a>() -> impl Parser<ParseState<'a>, Target = Term> {
        fn parse_nega(s: &mut ParseState) -> Result<Term, ParseMsg> {
            (lexem('~').wrap() >> uop_term()).parse(s)
        }

        fn parse_paren(s: &mut ParseState) -> Result<Term, ParseMsg> {
            (lexem('(').wrap() >> term() << lexem(')')).parse(s)
        }

        let unit = lexem("unit").wrap() >> psc::pure(unit);
        let nega = ParseFn(parse_nega).wrap();
        let paren = ParseFn(parse_paren);
        let pat = pat().map(Pat::q);
        unit | nega | paren | pat
    }

    pub fn biop_term<'a>() -> impl Parser<ParseState<'a>, Target = Term> {
        fn parse_disjoint(s: &mut ParseState) -> Result<Term, ParseMsg> {
            let nega = uop_term().parse(s)?;
            lexem('|').parse(s)?;
            let disjoint = biop_term().parse(s)?;
            Ok(nega | disjoint)
        }

        fn parse_conjoin(s: &mut ParseState) -> Result<Term, ParseMsg> {
            let nega = uop_term().parse(s)?;
            lexem('&').parse(s)?;
            let conjoin = biop_term().parse(s)?;
            Ok(nega & conjoin)
        }

        fn parse_nega(s: &mut ParseState) -> Result<Term, ParseMsg> {
            uop_term().parse(s)
        }
        let disjoint = ParseFn(parse_disjoint).wrap();
        let conjoin = ParseFn(parse_conjoin);
        let nega = ParseFn(parse_nega);
        disjoint | conjoin | nega
    }

    pub fn term<'a>() -> impl Parser<ParseState<'a>, Target = Term> {
        biop_term()
    }
}

pub mod decl {
    use super::*;
    use crate::ast::Decl;

    pub fn decl<'a>() -> impl Parser<ParseState<'a>, Target = Decl> {
        (pat_phase::pat().wrap() << lexem("=>")).map2(term_phase::term(), Decl::new)
    }

    pub fn decls<'a>() -> impl Parser<ParseState<'a>, Target = Vec<Decl>> {
        (decl().wrap() << lexem(';')).many().wrap() << psc::eof() //.snoc(decl().wrap() << lexem('.'))
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{arr, num, slice, string, var};
    use crate::parse::grammar::decl;
    use crate::parse::grammar::pat_phase::pat;
    use crate::parse::grammar::term_phase::term;
    use psc::{ParseState, Parser};

    #[test]
    fn test_pat() {
        let mut src = ParseState::new("\"12  \\ 3\"");
        let res = pat().parse(&mut src).unwrap();
        let p = string("12  \\ 3");
        assert_eq!(res, p);

        let mut src = ParseState::new("?-x_-8781d-sf");
        let res = pat().parse(&mut src).unwrap();
        let p = var("-x_-8781d-sf");
        assert_eq!(res, p);

        let mut src = ParseState::new("-123193");
        let res = pat().parse(&mut src).unwrap();
        let p = num(-123193);
        assert_eq!(res, p);

        let mut src = ParseState::new("[[1,2, ...?x], 1, \"\", ?x]");
        let res = pat().parse(&mut src).unwrap();
        let p = arr(vec![
            slice(vec![num(1), num(2)], "x"),
            num(1),
            string(""),
            var("x"),
        ]);
        assert_eq!(res, p);
    }

    #[test]
    fn test_term() {
        let mut src = ParseState::new("([?x, ...?x] & ?x) | \"123\"");
        let res = term().parse(&mut src).unwrap();
        let t = (slice(vec![var("x")], "x").q() & var("x").q()) | string("123").q();
        assert_eq!(res, t);

        let mut src = ParseState::new("[?x, ...?x] & ?x | \"123\"");
        let res = term().parse(&mut src).unwrap();
        let t = slice(vec![var("x")], "x").q() & (var("x").q() | string("123").q());
        assert_eq!(res, t);
    }

    #[test]
    fn test_decl() {
        let mut src = ParseState::new("?main => ([?x, ...?x] & ?x) | \"123\"");
        let res = decl::decl().parse(&mut src).unwrap();
        let decl = var("main")
            .expand_to((slice(vec![var("x")], "x").q() & var("x").q()) | string("123").q());
        assert_eq!(res, decl);

        let mut src = ParseState::new(
            "?main => ([?x, ...?x] & ?x) | \"123\";\n?main => ([?x, ...?x] & ?x) | \"123\";",
        );
        let res = decl::decls().parse(&mut src).unwrap();
        let decl = var("main")
            .expand_to((slice(vec![var("x")], "x").q() & var("x").q()) | string("123").q());
        assert_eq!(res, vec![decl.clone(), decl]);
    }
}
