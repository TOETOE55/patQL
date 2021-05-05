use crate::token::{Pos, Span};
use std::ops::Range;
use unicode_segmentation::*;
use unicode_width::UnicodeWidthStr;

pub type Result<T> = anyhow::Result<T>;

pub trait Parse<'a>: Sized {
    fn parse(stream: &mut ParseStream<'a>) -> Result<Self>;
}

#[derive(Clone)]
pub struct ParseStream<'a> {
    cursor_start: UWordBoundIndices<'a>,
    cursor_end: UWordBoundIndices<'a>,
    pub(crate) word_span: Span,

    pub(crate) range: Range<usize>,
}

impl<'a> ParseStream<'a> {
    pub fn new(source: &'a str) -> Self {
        let cursor_start = source.split_word_bound_indices();
        let mut cursor_end = cursor_start.clone();
        let span = if let Some((idx, word)) = cursor_end.next() {
            Span {
                end: Pos {
                    row: 0,
                    col: word.width(),
                    idx,
                },
                ..Span::default()
            }
        } else {
            Span::default()
        };

        Self {
            cursor_start,
            cursor_end,
            word_span: span,

            range: 0..source.len(),
        }
    }
}

impl<'a> ParseStream<'a> {
    pub fn parse<P: Parse<'a>>(&mut self) -> Result<P> {
        P::parse(self)
    }

    pub fn lookahead<P: Parse<'a>>(&self) -> Result<P> {
        P::parse(&mut self.clone())
    }

    pub fn lookahead_word(&self) -> Option<(Span, &'a str)> {
        self.clone().next()
    }

    pub fn peek<P: Parse<'a>>(&self) -> bool {
        P::parse(&mut self.clone()).is_ok()
    }

    pub fn peek2<P1: Parse<'a>, P2: Parse<'a>>(&self) -> bool {
        let mut s = self.clone();

        P1::parse(&mut s).is_ok() && P2::parse(&mut s).is_ok()
    }

    pub fn peek_str(&self, haystack: &str) -> bool {
        self.cursor_start.as_str().starts_with(haystack)
    }
}

impl<'a> Iterator for ParseStream<'a> {
    type Item = (Span, &'a str);

    fn next(&mut self) -> Option<Self::Item> {
        let (start, word) = self.cursor_start.next()?;
        let (end, _) = self.cursor_end.next()?;
        if end >= self.range.end {
            return None;
        }
        self.word_span.start.idx = start;
        self.word_span.end.idx = end;
        self.word_span.start.col = self.word_span.end.col;
        self.word_span.start.row = self.word_span.end.row;
        self.word_span.end.col = match word {
            "\n" | "\r\n" => {
                self.word_span.end.row += 1;
                0
            }
            "\t" => self.word_span.end.col + 8 - (self.word_span.end.col - 1) % 8,
            _ => self.word_span.end.col + word.width(),
        };

        Some((self.word_span, word))
    }
}
