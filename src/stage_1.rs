//! This file contains the initial lexing stage, which breaks up characters into distinct tokens.

use std::fmt::Write as _;

use chumsky::prelude::*;
use chumsky::text::Char;
use serde::Serialize;
use unicode_categories::UnicodeCategories;

/// Describes an individual part of the document.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize)]
pub enum NorgToken {
    Whitespace(u16),
    SingleNewline,
    Newlines(u16),
    Regular(char),
    Text(String),
    Special(char),
    Escape(char),
    End(char),
    Eof,
}

impl std::fmt::Display for NorgToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::End(c) => write!(f, "{}end", c),
            Self::Eof => f.write_char('\0'),
            Self::Escape(c) => write!(f, "\\{}", c),
            Self::Newlines(count) => f.write_str(&"\n".repeat(*count as usize)),
            Self::Regular(c) | Self::Special(c) => f.write_char(*c),
            Self::SingleNewline => f.write_char('\n'),
            Self::Text(s) => f.write_str(s),
            Self::Whitespace(count) => f.write_str(&" ".repeat(*count as usize)),
        }
    }
}

impl From<NorgToken> for String {
    fn from(value: NorgToken) -> Self {
        value.to_string()
    }
}

/// A list of characters which are considered "special", i.e. for parsing of attached modifiers.
const SPECIAL_CHARS: &str = "*-~/_!%^,\"'`$:@|=.#+<>()[]{}\\";

/// List of chars that proceed "end" tags are handled inline in the is_tag_char check.

fn coalesce_regular_chars(mut tokens: Vec<NorgToken>) -> Vec<NorgToken> {
    let mut result = Vec::with_capacity(tokens.len());
    let mut text_buf = String::new();
    for token in tokens.drain(..) {
        match token {
            NorgToken::Regular(c) => {
                text_buf.push(c);
            }
            other => {
                if !text_buf.is_empty() {
                    result.push(NorgToken::Text(std::mem::take(&mut text_buf)));
                }
                result.push(other);
            }
        }
    }
    if !text_buf.is_empty() {
        result.push(NorgToken::Text(text_buf));
    }
    result
}

/// Parses a `.norg` document and breaks it up into tokens.
pub fn stage_1<'src>() -> impl Parser<'src, &'src str, Vec<NorgToken>, extra::Err<Rich<'src, char>>> {
    custom::<'src, _, &'src str, Vec<NorgToken>, extra::Err<Rich<'src, char>>>(|inp| {
        let is_whitespace = |c: char| c.is_inline_whitespace() || c.is_separator_space();
        let is_newline =
            |c: char| c == '\n' || c == '\r' || c.is_separator_line() || c.is_separator_paragraph();
        let is_tag_char = |c: char| matches!(c, '|' | '@' | '=');

        let mut tokens = Vec::new();

        loop {
            let Some(c) = inp.next() else {
                tokens.push(NorgToken::Eof);
                return Ok(tokens);
            };

            if is_tag_char(c) {
                let saved = inp.save();

                let is_end = inp.peek() == Some('e')
                    && { inp.next(); inp.peek() == Some('n') }
                    && { inp.next(); inp.peek() == Some('d') };

                let mut is_tag_end = false;
                if is_end {
                    inp.next();
                    let after = inp.peek();
                    is_tag_end = after.map_or(true, |nc| is_newline(nc));
                }

                if is_tag_end {
                    tokens.push(NorgToken::End(c));
                    continue;
                }
                inp.rewind(saved);
            }

            if c == '\\' {
                if let Some(escaped) = inp.next() {
                    tokens.push(NorgToken::Escape(escaped));
                } else {
                    tokens.push(NorgToken::Special('\\'));
                }
                continue;
            }

            if is_newline(c) {
                let mut count = 1u16;
                while inp.peek().map_or(false, |nc| is_newline(nc)) {
                    inp.next();
                    count += 1;
                }
                if count == 1 {
                    tokens.push(NorgToken::SingleNewline);
                } else {
                    tokens.push(NorgToken::Newlines(count));
                }
                continue;
            }

            if is_whitespace(c) {
                let mut count = 1u16;
                while inp.peek().map_or(false, |nc| is_whitespace(nc)) {
                    inp.next();
                    count += 1;
                }
                tokens.push(NorgToken::Whitespace(count));
                continue;
            }

            if SPECIAL_CHARS.contains(c) {
                tokens.push(NorgToken::Special(c));
                continue;
            }

            // Regular character, will be coalesced into Text runs at the end
            tokens.push(NorgToken::Regular(c));
        }
    })
    .map(coalesce_regular_chars)
}
