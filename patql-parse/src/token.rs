use crate::parse::*;
use crate::token::group::{Brace, Bracket, Group, Paren};
use crate::token::helper::{Delimiter, WhiteSpace};
use crate::token::keyword::Keyword;
use crate::token::lit::Lit;
use crate::token::punctuate::Punct;
use lens_rs::*;
use std::marker::PhantomData;
use unicode_normalization::UnicodeNormalization;

#[derive(Copy, Clone, Eq, PartialEq, Debug, Default)]
pub struct Pos {
    pub row: usize,
    pub col: usize,
    pub idx: usize,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Default)]
pub struct Span {
    pub start: Pos,
    pub end: Pos,
}

#[derive(Clone, Debug)]
pub struct Ident {
    pub span: Span,
    pub name: String,
}

impl PartialEq for Ident {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Eq for Ident {}

impl Parse<'_> for Ident {
    fn parse(stream: &mut ParseStream) -> Result<Self> {
        stream.parse::<WhiteSpace>()?;

        if stream.peek::<Lit>()
            || stream.peek::<Keyword>()
            || stream.peek::<Punct>()
            || stream.peek::<Delimiter>()
        {
            let (word, span) = stream.next().unwrap();
            return Err(anyhow::anyhow!(
                "expected indent found `{:?}` in {:?}",
                word,
                span
            ));
        }

        let (mut span, mut buf) = stream
            .next()
            .map(|(span, word)| (span, word.to_string()))
            .ok_or_else(|| anyhow::anyhow!("expected indent"))?;
        while let Some((word_span, word)) = stream.lookahead_word() {
            if word.starts_with(char::is_whitespace) {
                break;
            }

            if stream.peek::<Punct>() || stream.peek::<Delimiter>() {
                break;
            }

            stream.next();
            span.end = word_span.end;
            buf += word;
        }

        stream.parse::<WhiteSpace>()?;

        Ok(Self {
            span,
            name: buf.nfc().collect(),
        })
    }
}

pub mod lit {
    use super::*;
    use anyhow::{anyhow, Context};

    #[derive(Clone, Eq, PartialEq, Debug, Prism, Review)]
    pub enum Lit {
        #[optic]
        Int(LitInt),
        #[optic]
        Bool(LitBool),
        #[optic]
        Str(LitStr),
    }

    #[derive(Copy, Clone, Debug)]
    pub struct LitInt {
        pub span: Span,
        pub value: i64,
    }

    impl PartialEq for LitInt {
        fn eq(&self, other: &Self) -> bool {
            self.value == other.value
        }
    }

    impl Eq for LitInt {}

    #[derive(Copy, Clone, Debug)]
    pub struct LitBool {
        pub span: Span,
        pub value: bool,
    }

    impl PartialEq for LitBool {
        fn eq(&self, other: &Self) -> bool {
            self.value == other.value
        }
    }

    impl Eq for LitBool {}

    #[derive(Clone, Debug)]
    pub struct LitStr {
        pub span: Span,
        pub value: String,
    }

    impl PartialEq for LitStr {
        fn eq(&self, other: &Self) -> bool {
            self.value == other.value
        }
    }

    impl Eq for LitStr {}

    impl Parse<'_> for LitInt {
        fn parse(stream: &mut ParseStream) -> Result<Self> {
            let _ = stream.parse::<WhiteSpace>();
            let (span, word) = stream.next().ok_or_else(|| anyhow!("expected `int`"))?;
            let _ = stream.parse::<WhiteSpace>();
            Ok(Self {
                span,
                value: word
                    .parse()
                    .context(format!("expected `int` found `{}` in {:?}", word, span))?,
            })
        }
    }

    impl Parse<'_> for LitBool {
        fn parse(stream: &mut ParseStream) -> Result<Self> {
            let _ = stream.parse::<WhiteSpace>();
            let (span, word) = stream.next().ok_or_else(|| anyhow!("expected `bool`"))?;
            let _ = stream.parse::<WhiteSpace>();
            Ok(Self {
                span,
                value: word
                    .parse()
                    .context(format!("expected `bool` found `{}` in {:?}", word, span))?,
            })
        }
    }

    impl Parse<'_> for LitStr {
        fn parse(stream: &mut ParseStream) -> Result<Self> {
            let _ = stream.parse::<WhiteSpace>();

            let (span, quote) = stream.next().ok_or_else(|| anyhow!("expected `string`"))?;
            if quote != "\"" {
                return Err(anyhow!("expected `string` found `{}` in {:?}", quote, span));
            }
            let mut buf = String::new();
            let end = loop {
                let (span, word) = stream.next().ok_or_else(|| anyhow!("expected `string`"))?;
                match word {
                    "\"" => break span.end,
                    "\\" => {
                        let (_, escape) =
                            stream.next().ok_or_else(|| anyhow!("expected `string`"))?;
                        buf += escape;
                    }
                    ctrl if ctrl.starts_with(char::is_control) => {
                        return Err(anyhow!("expected `\"` found `{}` in {:?}", word, span));
                    }
                    _ => buf += word,
                }
            };

            let _ = stream.parse::<WhiteSpace>();
            Ok(Self {
                span: Span { end, ..span },
                value: buf,
            })
        }
    }

    impl Lit {
        pub(crate) fn span(&self) -> Span {
            match self {
                Lit::Int(LitInt { span, .. })
                | Lit::Bool(LitBool { span, .. })
                | Lit::Str(LitStr { span, .. }) => *span,
            }
        }
    }

    impl Parse<'_> for Lit {
        fn parse(stream: &mut ParseStream) -> Result<Self> {
            if stream.peek::<LitInt>() {
                Ok(Lit::Int(stream.parse()?))
            } else if stream.peek::<LitBool>() {
                Ok(Lit::Bool(stream.parse()?))
            } else if stream.peek::<LitStr>() {
                Ok(Lit::Str(stream.parse()?))
            } else {
                let (span, word) = stream.next().ok_or_else(|| anyhow!("expected literal"))?;
                Err(anyhow!("expected literal found `{}` in {:?}", word, span))
            }
        }
    }
}

pub mod keyword {
    use super::*;
    use anyhow::anyhow;

    #[derive(Clone, Eq, PartialEq, Debug, Prism, Review)]
    pub enum Keyword {
        #[optic]
        For(self::For),
        #[optic]
        Any(self::Any),
        #[optic]
        All(self::All),
    }

    /// for
    #[derive(Copy, Clone, Debug)]
    pub struct For {
        pub span: Span,
    }

    impl PartialEq for For {
        fn eq(&self, _other: &Self) -> bool {
            true
        }
    }

    impl Eq for For {}

    /// any
    #[derive(Copy, Clone, Debug)]
    pub struct Any {
        pub span: Span,
    }

    impl PartialEq for Any {
        fn eq(&self, _other: &Self) -> bool {
            true
        }
    }

    impl Eq for Any {}

    /// for
    #[derive(Copy, Clone, Debug)]
    pub struct All {
        pub span: Span,
    }

    impl PartialEq for All {
        fn eq(&self, _other: &Self) -> bool {
            true
        }
    }

    impl Eq for All {}

    impl Parse<'_> for For {
        fn parse(stream: &mut ParseStream) -> Result<Self> {
            stream.parse::<WhiteSpace>()?;
            let (span, word) = stream.next().ok_or_else(|| anyhow!("expected `for`"))?;
            stream.parse::<WhiteSpace>()?;

            if word == "for" {
                Ok(Self { span })
            } else {
                Err(anyhow!("expected `for` found `{}` in {:?}", word, span))
            }
        }
    }

    impl Parse<'_> for Any {
        fn parse(stream: &mut ParseStream) -> Result<Self> {
            let _ = stream.parse::<WhiteSpace>();
            let (span, word) = stream.next().ok_or_else(|| anyhow!("expected `any`"))?;
            let _ = stream.parse::<WhiteSpace>();

            if word == "any" {
                Ok(Self { span })
            } else {
                Err(anyhow!("expected `any` found `{}` in {:?}", word, span))
            }
        }
    }

    impl Parse<'_> for All {
        fn parse(stream: &mut ParseStream) -> Result<Self> {
            let _ = stream.parse::<WhiteSpace>();
            let (span, word) = stream.next().ok_or_else(|| anyhow!("expected `all`"))?;
            let _ = stream.parse::<WhiteSpace>();

            if word == "all" {
                Ok(Self { span })
            } else {
                Err(anyhow!("expected `all` found `{}` in {:?}", word, span))
            }
        }
    }

    impl Keyword {
        pub(crate) fn span(&self) -> Span {
            match self {
                Keyword::For(For { span, .. })
                | Keyword::Any(Any { span, .. })
                | Keyword::All(All { span, .. }) => *span,
            }
        }
    }

    impl Parse<'_> for Keyword {
        fn parse(stream: &mut ParseStream) -> Result<Self> {
            if stream.peek::<For>() {
                Ok(Keyword::For(stream.parse()?))
            } else if stream.peek::<All>() {
                Ok(Keyword::All(stream.parse()?))
            } else if stream.peek::<Any>() {
                Ok(Keyword::Any(stream.parse()?))
            } else {
                let (span, word) = stream.next().ok_or_else(|| anyhow!("expected keyword"))?;
                Err(anyhow!("expected keyword found `{}` in {:?}", word, span))
            }
        }
    }
}

pub mod punctuate {
    use super::*;
    use crate::token::helper::WhiteSpace;
    use anyhow::anyhow;

    #[derive(Copy, Clone, Eq, PartialEq, Debug, Prism, Review)]
    pub enum Punct {
        #[optic]
        EQ(self::EQ),
        #[optic]
        NE(self::NE),
        #[optic]
        GT(self::GT),
        #[optic]
        GE(self::GE),
        #[optic]
        LT(self::LT),
        #[optic]
        LE(self::LE),
        #[optic]
        Add(self::Add),
        #[optic]
        Sub(self::Sub),
        #[optic]
        Mul(self::Mul),
        #[optic]
        Div(self::Div),
        #[optic]
        Rem(self::Rem),
        #[optic]
        And(self::And),
        #[optic]
        Or(self::Or),
        #[optic]
        Append(self::Append),
        #[optic]
        Bang(self::Bang),
        #[optic]
        Underscore(self::Underscore),
        #[optic]
        Colon(self::Colon),
        #[optic]
        Semi(self::Semi),
        #[optic]
        Comma(self::Comma),
        #[optic]
        Dot(self::Dot),
        #[optic]
        Dot2(self::Dot2),
        #[optic]
        At(self::At),
        #[optic]
        LArrow(self::LArrow),
    }

    /// ==
    #[derive(Copy, Clone, Debug)]
    pub struct EQ {
        pub span: Span,
    }

    impl PartialEq for EQ {
        fn eq(&self, _other: &Self) -> bool {
            true
        }
    }

    impl Eq for EQ {}

    impl Parse<'_> for EQ {
        fn parse(stream: &mut ParseStream) -> Result<Self> {
            let _ = stream.parse::<WhiteSpace>();

            let (span, eq1) = stream.next().ok_or_else(|| anyhow!("expected `==`"))?;
            let (Span { end, .. }, eq2) = stream.next().ok_or_else(|| anyhow!("expected `==`"))?;

            let _ = stream.parse::<WhiteSpace>();

            if (eq1, eq2) != ("=", "=") {
                return Err(anyhow!("expected `==` found `{}` in {:?}", eq1, span));
            }

            Ok(Self {
                span: Span { end, ..span },
            })
        }
    }

    /// !=
    #[derive(Copy, Clone, Debug)]
    pub struct NE {
        pub span: Span,
    }

    impl PartialEq for NE {
        fn eq(&self, _other: &Self) -> bool {
            true
        }
    }

    impl Eq for NE {}

    impl Parse<'_> for NE {
        fn parse(stream: &mut ParseStream) -> Result<Self> {
            let _ = stream.parse::<WhiteSpace>();

            let (span, bang) = stream.next().ok_or_else(|| anyhow!("expected `==`"))?;
            let (Span { end, .. }, eq) = stream.next().ok_or_else(|| anyhow!("expected `==`"))?;

            let _ = stream.parse::<WhiteSpace>();

            if (bang, eq) != ("!", "=") {
                return Err(anyhow!("expected `==` found `{}` in {:?}", bang, span));
            }

            Ok(Self {
                span: Span { end, ..span },
            })
        }
    }

    /// >=
    #[derive(Copy, Clone, Debug)]
    pub struct GE {
        pub span: Span,
    }

    impl PartialEq for GE {
        fn eq(&self, _other: &Self) -> bool {
            true
        }
    }

    impl Eq for GE {}

    impl Parse<'_> for GE {
        fn parse(stream: &mut ParseStream) -> Result<Self> {
            let _ = stream.parse::<WhiteSpace>();

            let (span, gt) = stream.next().ok_or_else(|| anyhow!("expected `>=`"))?;
            let (Span { end, .. }, eq) = stream.next().ok_or_else(|| anyhow!("expected `>=`"))?;

            let _ = stream.parse::<WhiteSpace>();

            if (gt, eq) != (">", "=") {
                return Err(anyhow!("expected `>=` found `{}` in {:?}", gt, span));
            }

            Ok(Self {
                span: Span { end, ..span },
            })
        }
    }

    /// >
    #[derive(Copy, Clone, Debug)]
    pub struct GT {
        pub span: Span,
    }

    impl PartialEq for GT {
        fn eq(&self, _other: &Self) -> bool {
            true
        }
    }

    impl Eq for GT {}

    impl Parse<'_> for GT {
        fn parse(stream: &mut ParseStream) -> Result<Self> {
            let _ = stream.parse::<WhiteSpace>();

            let (span, gt) = stream.next().ok_or_else(|| anyhow!("expected `>`"))?;

            let _ = stream.parse::<WhiteSpace>();

            if gt != ">" {
                return Err(anyhow!("expected `>` found `{}` in {:?}", gt, span));
            }

            Ok(Self { span })
        }
    }

    /// <
    #[derive(Copy, Clone, Debug)]
    pub struct LT {
        pub span: Span,
    }

    impl PartialEq for LT {
        fn eq(&self, _other: &Self) -> bool {
            true
        }
    }

    impl Eq for LT {}

    impl Parse<'_> for LT {
        fn parse(stream: &mut ParseStream) -> Result<Self> {
            let _ = stream.parse::<WhiteSpace>();

            let (span, lt) = stream.next().ok_or_else(|| anyhow!("expected `<`"))?;

            let _ = stream.parse::<WhiteSpace>();

            if lt != "<" {
                return Err(anyhow!("expected `<` found `{}` in {:?}", lt, span));
            }

            Ok(Self { span })
        }
    }

    /// <=
    #[derive(Copy, Clone, Debug)]
    pub struct LE {
        pub span: Span,
    }

    impl PartialEq for LE {
        fn eq(&self, _other: &Self) -> bool {
            true
        }
    }

    impl Eq for LE {}

    impl Parse<'_> for LE {
        fn parse(stream: &mut ParseStream) -> Result<Self> {
            let _ = stream.parse::<WhiteSpace>();

            let (span, lt) = stream.next().ok_or_else(|| anyhow!("expected `<=`"))?;
            let (Span { end, .. }, eq) = stream.next().ok_or_else(|| anyhow!("expected `<=`"))?;

            let _ = stream.parse::<WhiteSpace>();

            if (lt, eq) != ("<", "=") {
                return Err(anyhow!("expected `<=` found `{}` in {:?}", lt, span));
            }

            Ok(Self {
                span: Span { end, ..span },
            })
        }
    }

    /// +
    #[derive(Copy, Clone, Debug)]
    pub struct Add {
        pub span: Span,
    }

    impl PartialEq for Add {
        fn eq(&self, _other: &Self) -> bool {
            true
        }
    }

    impl Eq for Add {}

    impl Parse<'_> for Add {
        fn parse(stream: &mut ParseStream) -> Result<Self> {
            let _ = stream.parse::<WhiteSpace>();

            let (span, add) = stream.next().ok_or_else(|| anyhow!("expected `+`"))?;

            let _ = stream.parse::<WhiteSpace>();

            if add != "+" {
                return Err(anyhow!("expected `+` found `{}` in {:?}", add, span));
            }

            Ok(Self { span })
        }
    }

    /// -
    #[derive(Copy, Clone, Debug)]
    pub struct Sub {
        pub span: Span,
    }

    impl PartialEq for Sub {
        fn eq(&self, _other: &Self) -> bool {
            true
        }
    }

    impl Eq for Sub {}

    impl Parse<'_> for Sub {
        fn parse(stream: &mut ParseStream) -> Result<Self> {
            let _ = stream.parse::<WhiteSpace>();

            let (span, sub) = stream.next().ok_or_else(|| anyhow!("expected `-`"))?;

            let _ = stream.parse::<WhiteSpace>();

            if sub != "-" {
                return Err(anyhow!("expected `-` found `{}` in {:?}", sub, span));
            }

            Ok(Self { span })
        }
    }

    /// *
    #[derive(Copy, Clone, Debug)]
    pub struct Mul {
        pub span: Span,
    }

    impl PartialEq for Mul {
        fn eq(&self, _other: &Self) -> bool {
            true
        }
    }

    impl Eq for Mul {}

    impl Parse<'_> for Mul {
        fn parse(stream: &mut ParseStream) -> Result<Self> {
            let _ = stream.parse::<WhiteSpace>();

            let (span, mul) = stream.next().ok_or_else(|| anyhow!("expected `*`"))?;

            let _ = stream.parse::<WhiteSpace>();

            if mul != "*" {
                return Err(anyhow!("expected `*` found `{}` in {:?}", mul, span));
            }

            Ok(Self { span })
        }
    }

    /// /
    #[derive(Copy, Clone, Debug)]
    pub struct Div {
        pub span: Span,
    }

    impl PartialEq for Div {
        fn eq(&self, _other: &Self) -> bool {
            true
        }
    }

    impl Eq for Div {}

    impl Parse<'_> for Div {
        fn parse(stream: &mut ParseStream) -> Result<Self> {
            let _ = stream.parse::<WhiteSpace>();

            let (span, div) = stream.next().ok_or_else(|| anyhow!("expected `/`"))?;

            let _ = stream.parse::<WhiteSpace>();

            if div != "/" {
                return Err(anyhow!("expected `/` found `{}` in {:?}", div, span));
            }

            Ok(Self { span })
        }
    }

    /// %
    #[derive(Copy, Clone, Debug)]
    pub struct Rem {
        pub span: Span,
    }

    impl PartialEq for Rem {
        fn eq(&self, _other: &Self) -> bool {
            true
        }
    }

    impl Eq for Rem {}

    impl Parse<'_> for Rem {
        fn parse(stream: &mut ParseStream) -> Result<Self> {
            let _ = stream.parse::<WhiteSpace>();

            let (span, div) = stream.next().ok_or_else(|| anyhow!("expected `%`"))?;

            let _ = stream.parse::<WhiteSpace>();

            if div != "%" {
                return Err(anyhow!("expected `%` found `{}` in {:?}", div, span));
            }

            Ok(Self { span })
        }
    }

    /// &&
    #[derive(Copy, Clone, Debug)]
    pub struct And {
        pub span: Span,
    }

    impl PartialEq for And {
        fn eq(&self, _other: &Self) -> bool {
            true
        }
    }

    impl Eq for And {}

    impl Parse<'_> for And {
        fn parse(stream: &mut ParseStream) -> Result<Self> {
            let _ = stream.parse::<WhiteSpace>();

            let (span, bit_and1) = stream.next().ok_or_else(|| anyhow!("expected `&&`"))?;
            let (Span { end, .. }, bit_and2) = stream.next().ok_or(anyhow!("expected `&&`"))?;

            let _ = stream.parse::<WhiteSpace>();

            if (bit_and1, bit_and2) != ("&", "&") {
                return Err(anyhow!("expected `&&` found `{}` in {:?}", bit_and1, span));
            }

            Ok(Self {
                span: Span { end, ..span },
            })
        }
    }

    /// ||
    #[derive(Copy, Clone, Debug)]
    pub struct Or {
        pub span: Span,
    }

    impl PartialEq for Or {
        fn eq(&self, _other: &Self) -> bool {
            true
        }
    }

    impl Eq for Or {}

    impl Parse<'_> for Or {
        fn parse(stream: &mut ParseStream) -> Result<Self> {
            let _ = stream.parse::<WhiteSpace>();

            let (span, bit_or1) = stream.next().ok_or_else(|| anyhow!("expected `||`"))?;
            let (Span { end, .. }, bit_or2) =
                stream.next().ok_or_else(|| anyhow!("expected `||`"))?;

            let _ = stream.parse::<WhiteSpace>();

            if (bit_or1, bit_or2) != ("|", "|") {
                return Err(anyhow!("expected `||` found `{}` in {:?}", bit_or1, span));
            }

            Ok(Self {
                span: Span { end, ..span },
            })
        }
    }

    /// ++
    #[derive(Copy, Clone, Debug)]
    pub struct Append {
        pub span: Span,
    }

    impl PartialEq for Append {
        fn eq(&self, _other: &Self) -> bool {
            true
        }
    }

    impl Eq for Append {}

    impl Parse<'_> for Append {
        fn parse(stream: &mut ParseStream) -> Result<Self> {
            let _ = stream.parse::<WhiteSpace>();

            let (span, add1) = stream.next().ok_or_else(|| anyhow!("expected `++`"))?;
            let (Span { end, .. }, add2) = stream.next().ok_or_else(|| anyhow!("expected `++`"))?;

            let _ = stream.parse::<WhiteSpace>();

            if (add1, add2) != ("+", "+") {
                return Err(anyhow!("expected `++` found `{}` in {:?}", add1, span));
            }

            Ok(Self {
                span: Span { end, ..span },
            })
        }
    }

    /// !
    #[derive(Copy, Clone, Debug)]
    pub struct Bang {
        pub span: Span,
    }

    impl PartialEq for Bang {
        fn eq(&self, _other: &Self) -> bool {
            true
        }
    }

    impl Eq for Bang {}

    impl Parse<'_> for Bang {
        fn parse(stream: &mut ParseStream) -> Result<Self> {
            let _ = stream.parse::<WhiteSpace>();

            let (span, bang) = stream.next().ok_or_else(|| anyhow!("expected `!`"))?;
            if bang != "%" {
                return Err(anyhow!("expected `!` found `{}` in {:?}", bang, span));
            }

            Ok(Self { span })
        }
    }

    /// _
    #[derive(Copy, Clone, Debug)]
    pub struct Underscore {
        pub span: Span,
    }

    impl PartialEq for Underscore {
        fn eq(&self, _other: &Self) -> bool {
            true
        }
    }

    impl Eq for Underscore {}

    impl Parse<'_> for Underscore {
        fn parse(stream: &mut ParseStream) -> Result<Self> {
            let _ = stream.parse::<WhiteSpace>();

            let (span, underscore) = stream.next().ok_or_else(|| anyhow!("expected `_`"))?;

            let _ = stream.parse::<WhiteSpace>();

            if underscore != "_" {
                return Err(anyhow!("expected `_` found `{}` in {:?}", underscore, span));
            }

            Ok(Self { span })
        }
    }

    /// :
    #[derive(Copy, Clone, Debug)]
    pub struct Colon {
        pub span: Span,
    }

    impl PartialEq for Colon {
        fn eq(&self, _other: &Self) -> bool {
            true
        }
    }

    impl Eq for Colon {}

    impl Parse<'_> for Colon {
        fn parse(stream: &mut ParseStream) -> Result<Self> {
            let _ = stream.parse::<WhiteSpace>();

            let (span, colon) = stream.next().ok_or_else(|| anyhow!("expected `:`"))?;

            let _ = stream.parse::<WhiteSpace>();

            if colon != ":" {
                return Err(anyhow!("expected `:` found `{}` in {:?}", colon, span));
            }

            Ok(Self { span })
        }
    }

    /// ;
    #[derive(Copy, Clone, Debug)]
    pub struct Semi {
        pub span: Span,
    }

    impl PartialEq for Semi {
        fn eq(&self, _other: &Self) -> bool {
            true
        }
    }

    impl Eq for Semi {}

    impl Parse<'_> for Semi {
        fn parse(stream: &mut ParseStream) -> Result<Self> {
            let _ = stream.parse::<WhiteSpace>();

            let (span, semi) = stream.next().ok_or_else(|| anyhow!("expected `;`"))?;

            let _ = stream.parse::<WhiteSpace>();

            if semi != ";" {
                return Err(anyhow!("expected `;` found `{}` in {:?}", semi, span));
            }

            Ok(Self { span })
        }
    }

    /// ,
    #[derive(Copy, Clone, Debug)]
    pub struct Comma {
        pub span: Span,
    }

    impl PartialEq for Comma {
        fn eq(&self, _other: &Self) -> bool {
            true
        }
    }

    impl Eq for Comma {}

    impl Parse<'_> for Comma {
        fn parse(stream: &mut ParseStream) -> Result<Self> {
            let _ = stream.parse::<WhiteSpace>();

            let (span, comma) = stream.next().ok_or_else(|| anyhow!("expected `,`"))?;

            let _ = stream.parse::<WhiteSpace>();

            if comma != "," {
                return Err(anyhow!("expected `,` found `{}` in {:?}", comma, span));
            }

            Ok(Self { span })
        }
    }

    /// .
    #[derive(Copy, Clone, Debug)]
    pub struct Dot {
        pub span: Span,
    }

    impl PartialEq for Dot {
        fn eq(&self, _other: &Self) -> bool {
            true
        }
    }

    impl Eq for Dot {}

    impl Parse<'_> for Dot {
        fn parse(stream: &mut ParseStream) -> Result<Self> {
            let _ = stream.parse::<WhiteSpace>();

            let (span, dot) = stream.next().ok_or_else(|| anyhow!("expected `.`"))?;

            let _ = stream.parse::<WhiteSpace>();

            if dot != "." {
                return Err(anyhow!("expected `.` found `{}` in {:?}", dot, span));
            }

            Ok(Self { span })
        }
    }

    /// ..
    #[derive(Copy, Clone, Debug)]
    pub struct Dot2 {
        pub span: Span,
    }

    impl PartialEq for Dot2 {
        fn eq(&self, _other: &Self) -> bool {
            true
        }
    }

    impl Eq for Dot2 {}

    impl Parse<'_> for Dot2 {
        fn parse(stream: &mut ParseStream) -> Result<Self> {
            let _ = stream.parse::<WhiteSpace>();

            let (span, dot1) = stream.next().ok_or_else(|| anyhow!("expected `..`"))?;
            let (Span { end, .. }, dot2) = stream.next().ok_or_else(|| anyhow!("expected `..`"))?;

            let _ = stream.parse::<WhiteSpace>();

            if (dot1, dot2) != (".", ".") {
                return Err(anyhow!("expected `..` found `{}` in {:?}", dot1, span));
            }

            Ok(Self {
                span: Span { end, ..span },
            })
        }
    }

    /// @
    #[derive(Copy, Clone, Debug)]
    pub struct At {
        pub span: Span,
    }

    impl PartialEq for At {
        fn eq(&self, _other: &Self) -> bool {
            true
        }
    }

    impl Eq for At {}

    impl Parse<'_> for At {
        fn parse(stream: &mut ParseStream) -> Result<Self> {
            stream.parse::<WhiteSpace>()?;

            let (span, at) = stream.next().ok_or_else(|| anyhow!("expected `@`"))?;

            stream.parse::<WhiteSpace>()?;

            if at != "@" {
                return Err(anyhow!("expected `@` found `{}` in {:?}", at, span));
            }

            Ok(Self { span })
        }
    }

    /// <-
    #[derive(Copy, Clone, Debug)]
    pub struct LArrow {
        pub span: Span,
    }

    impl PartialEq for LArrow {
        fn eq(&self, _other: &Self) -> bool {
            true
        }
    }

    impl Eq for LArrow {}

    impl Parse<'_> for LArrow {
        fn parse(stream: &mut ParseStream) -> Result<Self> {
            let _ = stream.parse::<WhiteSpace>();

            let (span, lt) = stream.next().ok_or_else(|| anyhow!("expected `<-`"))?;
            let (Span { end, .. }, sub) = stream.next().ok_or_else(|| anyhow!("expected `<-`"))?;

            let _ = stream.parse::<WhiteSpace>();

            if (lt, sub) != ("<", "-") {
                return Err(anyhow!("expected `<-` found `{}` in {:?}", lt, span));
            }

            Ok(Self {
                span: Span { end, ..span },
            })
        }
    }

    impl Punct {
        pub(crate) fn span(&self) -> Span {
            match self {
                Punct::EQ(EQ { span })
                | Punct::NE(NE { span })
                | Punct::GT(GT { span })
                | Punct::GE(GE { span })
                | Punct::LT(LT { span })
                | Punct::LE(LE { span })
                | Punct::Add(Add { span })
                | Punct::Sub(Sub { span })
                | Punct::Mul(Mul { span })
                | Punct::Div(Div { span })
                | Punct::Rem(Rem { span })
                | Punct::And(And { span })
                | Punct::Or(Or { span })
                | Punct::Append(Append { span })
                | Punct::Bang(Bang { span })
                | Punct::Underscore(Underscore { span })
                | Punct::Colon(Colon { span })
                | Punct::Semi(Semi { span })
                | Punct::Comma(Comma { span })
                | Punct::Dot(Dot { span })
                | Punct::Dot2(Dot2 { span })
                | Punct::At(At { span })
                | Punct::LArrow(LArrow { span }) => *span,
            }
        }
    }

    impl Parse<'_> for Punct {
        fn parse(stream: &mut ParseStream<'_>) -> Result<Self> {
            if stream.peek::<EQ>() {
                Ok(Self::EQ(stream.parse()?))
            } else if stream.peek::<NE>() {
                Ok(Self::NE(stream.parse()?))
            } else if stream.peek::<LArrow>() {
                Ok(Self::LArrow(stream.parse()?))
            } else if stream.peek::<GE>() {
                Ok(Self::GE(stream.parse()?))
            } else if stream.peek::<GT>() {
                Ok(Self::GT(stream.parse()?))
            } else if stream.peek::<LE>() {
                Ok(Self::LE(stream.parse()?))
            } else if stream.peek::<LT>() {
                Ok(Self::LT(stream.parse()?))
            } else if stream.peek::<Append>() {
                Ok(Self::Append(stream.parse()?))
            } else if stream.peek::<Add>() {
                Ok(Self::Add(stream.parse()?))
            } else if stream.peek::<Sub>() {
                Ok(Self::Sub(stream.parse()?))
            } else if stream.peek::<Mul>() {
                Ok(Self::Mul(stream.parse()?))
            } else if stream.peek::<Div>() {
                Ok(Self::Div(stream.parse()?))
            } else if stream.peek::<Rem>() {
                Ok(Self::Rem(stream.parse()?))
            } else if stream.peek::<And>() {
                Ok(Self::And(stream.parse()?))
            } else if stream.peek::<Or>() {
                Ok(Self::Or(stream.parse()?))
            } else if stream.peek::<Bang>() {
                Ok(Self::Bang(stream.parse()?))
            } else if stream.peek::<Underscore>() {
                Ok(Self::Underscore(stream.parse()?))
            } else if stream.peek::<Colon>() {
                Ok(Self::Colon(stream.parse()?))
            } else if stream.peek::<Comma>() {
                Ok(Self::Comma(stream.parse()?))
            } else if stream.peek::<Semi>() {
                Ok(Self::Semi(stream.parse()?))
            } else if stream.peek::<At>() {
                Ok(Self::At(stream.parse()?))
            } else if stream.peek::<Dot2>() {
                Ok(Self::Dot2(stream.parse()?))
            } else if stream.peek::<Dot>() {
                Ok(Self::Dot(stream.parse()?))
            } else {
                let (span, word) = stream.next().ok_or_else(|| anyhow!("expected keyword"))?;
                Err(anyhow!("expected punct found `{}` in {:?}", word, span))
            }
        }
    }

    //...
}

pub mod group {
    use super::*;
    use crate::token::helper::{LBrace, LBracket, LParen};
    use anyhow::anyhow;
    use std::fmt::{Debug, Formatter};

    /// (...)
    #[derive(Clone)]
    pub struct Paren<'a> {
        pub span: Span,
        pub sub_stream: ParseStream<'a>,
    }

    impl<'a> Parse<'a> for Paren<'a> {
        fn parse(stream: &mut ParseStream<'a>) -> Result<Self> {
            stream.parse::<WhiteSpace>()?;

            let (start_span, lb) = stream.next().ok_or_else(|| anyhow!("expected `(`"))?;
            if lb != "(" {
                return Err(anyhow!("expected `(` found `{}` in {:?}", lb, start_span));
            }

            let mut sub_stream: ParseStream<'a> = stream.clone();

            let end_span = loop {
                stream.parse::<WhiteSpace>()?;
                if stream.peek::<TokenTree>() {
                    stream.parse::<TokenTree>()?;
                } else {
                    let (span, rb) = stream.next().ok_or_else(|| anyhow!("expected `)`"))?;
                    if rb != ")" {
                        return Err(anyhow!("expected `)` found `{}` in {:?}", rb, span));
                    }
                    break span;
                }
            };

            stream.parse::<WhiteSpace>()?;

            sub_stream.range.start = start_span.end.idx;
            sub_stream.range.end = end_span.start.idx;

            Ok(Self {
                span: Span {
                    end: end_span.end,
                    ..start_span
                },
                sub_stream,
            })
        }
    }

    impl Debug for Paren<'_> {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            write!(f, "(..{:?}..)", self.span)
        }
    }

    /// [...]
    #[derive(Clone)]
    pub struct Bracket<'a> {
        pub span: Span,
        pub sub_stream: ParseStream<'a>,
    }

    impl<'a> Parse<'a> for Bracket<'a> {
        fn parse(stream: &mut ParseStream<'a>) -> Result<Self> {
            stream.parse::<WhiteSpace>()?;

            let (start_span, lb) = stream.next().ok_or_else(|| anyhow!("expected `[`"))?;
            if lb != "[" {
                return Err(anyhow!("expected `[` found `{}` in {:?}", lb, start_span));
            }

            let mut sub_stream: ParseStream<'a> = stream.clone();

            let end_span = loop {
                stream.parse::<WhiteSpace>()?;
                if stream.peek::<TokenTree>() {
                    stream.parse::<TokenTree>()?;
                } else {
                    let (span, rb) = stream.next().ok_or_else(|| anyhow!("expected `]`"))?;
                    if rb != "]" {
                        return Err(anyhow!("expected `]` found `{}` in {:?}", rb, span));
                    }
                    break span;
                }
            };

            stream.parse::<WhiteSpace>()?;

            sub_stream.range.start = start_span.end.idx;
            sub_stream.range.end = end_span.start.idx;

            Ok(Self {
                span: Span {
                    end: end_span.end,
                    ..start_span
                },
                sub_stream,
            })
        }
    }

    impl Debug for Bracket<'_> {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            write!(f, "[..{:?}..]", self.span)
        }
    }

    /// {...}
    #[derive(Clone)]
    pub struct Brace<'a> {
        pub span: Span,
        pub sub_stream: ParseStream<'a>,
    }

    impl Debug for Brace<'_> {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            write!(f, "{{..{:?}..}}", self.span)
        }
    }

    impl<'a> Parse<'a> for Brace<'a> {
        fn parse(stream: &mut ParseStream<'a>) -> Result<Self> {
            stream.parse::<WhiteSpace>()?;

            let (start_span, lb) = stream.next().ok_or_else(|| anyhow!("expected `{{`"))?;
            if lb != "{" {
                return Err(anyhow!("expected `{{` found `{}` in {:?}", lb, start_span));
            }

            let mut sub_stream: ParseStream<'a> = stream.clone();

            let end_span = loop {
                stream.parse::<WhiteSpace>()?;
                if stream.peek::<TokenTree>() {
                    stream.parse::<TokenTree>()?;
                } else {
                    let (span, rb) = stream.next().ok_or_else(|| anyhow!("expected `}}`"))?;
                    if rb != "}" {
                        return Err(anyhow!("expected `}}` found `{}` in {:?}", rb, span));
                    }
                    break span;
                }
            };

            stream.parse::<WhiteSpace>()?;

            sub_stream.range.start = start_span.end.idx;
            sub_stream.range.end = end_span.start.idx;

            Ok(Self {
                span: Span {
                    end: end_span.end,
                    ..start_span
                },
                sub_stream,
            })
        }
    }

    #[derive(Clone, Debug)]
    pub enum Group<'a> {
        Paren(self::Paren<'a>),
        Bracket(self::Bracket<'a>),
        Brace(self::Brace<'a>),
    }

    impl Group<'_> {
        pub fn span(&self) -> Span {
            match self {
                Group::Paren(Paren { span, .. })
                | Group::Bracket(Bracket { span, .. })
                | Group::Brace(Brace { span, .. }) => *span,
            }
        }
    }

    impl<'a> Parse<'a> for Group<'a> {
        fn parse(stream: &mut ParseStream<'a>) -> Result<Self> {
            if stream.peek::<LParen>() {
                Ok(Self::Paren(stream.parse()?))
            } else if stream.peek::<LBracket>() {
                Ok(Self::Bracket(stream.parse()?))
            } else if stream.peek::<LBrace>() {
                Ok(Self::Brace(stream.parse()?))
            } else {
                let (span, word) = stream
                    .next()
                    .ok_or_else(|| anyhow!("expected delimiters"))?;
                Err(anyhow!("expected groups found `{}` in {:?}", word, span))
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum TokenTree<'a> {
    Ident(self::Ident),
    Lit(lit::Lit),
    Keyword(keyword::Keyword),
    Punct(punctuate::Punct),
    Group(group::Group<'a>),
}

impl<'a> TokenTree<'a> {
    pub fn span(&self) -> Span {
        match self {
            TokenTree::Ident(Ident { span, .. }) => *span,
            TokenTree::Keyword(keyword) => keyword.span(),
            TokenTree::Punct(punct) => punct.span(),
            TokenTree::Group(group) => group.span(),
            TokenTree::Lit(lit) => lit.span(),
        }
    }
}

impl<'a> Parse<'a> for TokenTree<'a> {
    fn parse(stream: &mut ParseStream<'a>) -> Result<Self> {
        if stream.peek::<Ident>() {
            Ok(Self::Ident(stream.parse()?))
        } else if stream.peek::<Keyword>() {
            Ok(Self::Keyword(stream.parse()?))
        } else if stream.peek::<Punct>() {
            Ok(Self::Punct(stream.parse()?))
        } else if stream.peek::<Lit>() {
            Ok(Self::Lit(stream.parse()?))
        } else {
            Ok(Self::Group(stream.parse()?))
        }
    }
}

pub mod layout {
    use super::*;
    use std::fmt::{Debug, Formatter};

    ///   ...
    ///   ...
    #[derive(Clone)]
    pub struct Align<'a> {
        pub span: Span,
        pub sub_streams: Vec<ParseStream<'a>>,
    }

    impl Debug for Align<'_> {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            write!(f, "<align>..{:?}..<align/>", self.span)
        }
    }

    impl<'a> Parse<'a> for Align<'a> {
        fn parse(stream: &mut ParseStream<'a>) -> Result<Self> {
            let mut align_span = stream.lookahead::<TokenTree>()?.span();
            let align = align_span.start.col;

            let mut sub_streams = vec![];

            'outer: loop {
                let mut sub_stream = stream.clone();
                let mut sub_span;
                if let Ok(tok) = stream.parse::<TokenTree>() {
                    sub_span = tok.span();
                } else {
                    break 'outer;
                }
                'inner: loop {
                    if let Ok(tok) = stream.lookahead::<TokenTree>() {
                        if tok.span().start.col < align {
                            break 'outer;
                        }

                        if tok.span().start.col == align {
                            break 'inner;
                        }
                        stream.parse::<TokenTree>()?;
                        sub_span.end = tok.span().end;
                    } else {
                        break 'inner;
                    }
                }
                sub_stream.range.start = sub_span.start.idx;
                sub_stream.range.end = sub_span.end.idx;
                sub_streams.push(sub_stream);
                align_span.end = sub_span.end;
            }

            Ok(Self {
                span: align_span,
                sub_streams,
            })
        }
    }
}

pub mod helper {
    use super::*;
    #[derive(Clone, Debug, Copy)]
    pub struct Delimiter;

    impl Parse<'_> for Delimiter {
        fn parse(stream: &mut ParseStream<'_>) -> Result<Self> {
            stream.parse::<WhiteSpace>()?;
            let (span, word) = stream
                .next()
                .ok_or_else(|| anyhow::anyhow!("expected `{{`, `}}`, `[`, `]`, `(`, `)`"))?;
            if !(word == "("
                || word == ")"
                || word == "["
                || word == "]"
                || word == "{"
                || word == "}")
            {
                return Err(anyhow::anyhow!(
                    "expected `{{`, `}}`, `[`, `]`, `(`, `)` found `{:?}` in {:?}",
                    word,
                    span
                ));
            }
            stream.parse::<WhiteSpace>()?;
            Ok(Delimiter)
        }
    }

    #[derive(Clone, Debug, Copy)]
    pub struct LParen;
    #[derive(Clone, Debug, Copy)]
    pub struct RParen;
    #[derive(Clone, Debug, Copy)]
    pub struct LBracket;
    #[derive(Clone, Debug, Copy)]
    pub struct RBracket;
    #[derive(Clone, Debug, Copy)]
    pub struct LBrace;
    #[derive(Clone, Debug, Copy)]
    pub struct RBrace;

    impl Parse<'_> for LParen {
        fn parse(stream: &mut ParseStream<'_>) -> Result<Self> {
            stream.parse::<WhiteSpace>()?;
            let (span, word) = stream
                .next()
                .ok_or_else(|| anyhow::anyhow!("expected `(`"))?;
            if word != "(" {
                return Err(anyhow::anyhow!(
                    "expected `(` found `{:?}` in {:?}",
                    word,
                    span
                ));
            }
            stream.parse::<WhiteSpace>()?;
            Ok(LParen)
        }
    }

    impl Parse<'_> for RParen {
        fn parse(stream: &mut ParseStream<'_>) -> Result<Self> {
            stream.parse::<WhiteSpace>()?;
            let (span, word) = stream
                .next()
                .ok_or_else(|| anyhow::anyhow!("expected `)`"))?;
            if word != ")" {
                return Err(anyhow::anyhow!(
                    "expected `)` found `{:?}` in {:?}",
                    word,
                    span
                ));
            }
            stream.parse::<WhiteSpace>()?;
            Ok(RParen)
        }
    }

    impl Parse<'_> for LBracket {
        fn parse(stream: &mut ParseStream<'_>) -> Result<Self> {
            stream.parse::<WhiteSpace>()?;
            let (span, word) = stream
                .next()
                .ok_or_else(|| anyhow::anyhow!("expected `[`"))?;
            if word != "[" {
                return Err(anyhow::anyhow!(
                    "expected `[` found `{:?}` in {:?}",
                    word,
                    span
                ));
            }
            stream.parse::<WhiteSpace>()?;
            Ok(LBracket)
        }
    }

    impl Parse<'_> for RBracket {
        fn parse(stream: &mut ParseStream<'_>) -> Result<Self> {
            stream.parse::<WhiteSpace>()?;
            let (span, word) = stream
                .next()
                .ok_or_else(|| anyhow::anyhow!("expected `]`"))?;
            if word != "]" {
                return Err(anyhow::anyhow!(
                    "expected `]` found `{:?}` in {:?}",
                    word,
                    span
                ));
            }
            stream.parse::<WhiteSpace>()?;
            Ok(RBracket)
        }
    }

    impl Parse<'_> for LBrace {
        fn parse(stream: &mut ParseStream<'_>) -> Result<Self> {
            stream.parse::<WhiteSpace>()?;
            let (span, word) = stream
                .next()
                .ok_or_else(|| anyhow::anyhow!("expected `{{`"))?;
            if word != "{" {
                return Err(anyhow::anyhow!(
                    "expected `{{` found `{:?}` in {:?}",
                    word,
                    span
                ));
            }
            stream.parse::<WhiteSpace>()?;
            Ok(LBrace)
        }
    }

    impl Parse<'_> for RBrace {
        fn parse(stream: &mut ParseStream<'_>) -> Result<Self> {
            stream.parse::<WhiteSpace>()?;
            let (span, word) = stream
                .next()
                .ok_or_else(|| anyhow::anyhow!("expected `}}`"))?;
            if word != "}}" {
                return Err(anyhow::anyhow!(
                    "expected `}}` found `{:?}` in {:?}",
                    word,
                    span
                ));
            }
            stream.parse::<WhiteSpace>()?;
            Ok(RBrace)
        }
    }

    /// zero or more white space
    #[derive(Clone, Debug, Copy)]
    pub struct WhiteSpace;

    impl Parse<'_> for WhiteSpace {
        fn parse(stream: &mut ParseStream<'_>) -> Result<Self> {
            while let Some((_, word)) = stream.lookahead_word() {
                if !word.starts_with(char::is_whitespace) {
                    break;
                }
                stream.next();
            }

            Ok(WhiteSpace)
        }
    }

    #[derive(Debug)]
    pub struct Punctuated<T, P> {
        pub items: Vec<T>,
        _marker: PhantomData<Vec<(T, P)>>,
    }

    impl<T: Clone, P> Clone for Punctuated<T, P> {
        fn clone(&self) -> Self {
            Self {
                items: self.items.clone(),
                _marker: Default::default(),
            }
        }
    }

    impl<'a, T: Parse<'a>, P: Parse<'a>> Parse<'a> for Punctuated<T, P> {
        fn parse(stream: &mut ParseStream<'a>) -> Result<Self> {
            let mut items = vec![];
            if stream.peek::<P>() {
                stream.parse::<P>()?;
                return Ok(Self {
                    items,
                    _marker: Default::default(),
                });
            }

            while stream.peek2::<T, P>() {
                items.push(stream.parse::<T>()?);
                stream.parse::<P>()?;
            }

            if stream.peek::<T>() {
                items.push(stream.parse::<T>()?);
            }

            Ok(Self {
                items,
                _marker: Default::default(),
            })
        }
    }
}
