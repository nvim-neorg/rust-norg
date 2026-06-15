//! This file contains the initial lexing stage, which breaks up characters into distinct tokens.

use std::fmt::Write as _;

use chumsky::prelude::*;
use chumsky::text::{keyword, Char};
use serde::Serialize;
use unicode_categories::UnicodeCategories;

/// Describes an individual part of the document.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize)]
pub enum NorgToken {
    Whitespace(u16),
    SingleNewline,
    Newlines(u16),
    Regular(char),
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

/// List of chars that proceed "end" tags
const TAG_CHARS: &str = "|@=";

/// Parses a `.norg` document and breaks it up into tokens.
pub fn stage_1<'src>() -> impl Parser<'src, &'src str, Vec<NorgToken>, extra::Err<Rich<'src, char>>> {
    let ws = any::<_, extra::Err<Rich<char>>>()
        .filter(|c: &char| c.is_inline_whitespace() || c.is_separator_space())
        .repeated()
        .at_least(1)
        .collect::<String>()
        .map(|s| NorgToken::Whitespace(s.len() as u16));

    let character = any::<_, extra::Err<Rich<char>>>().map(NorgToken::Regular);

    let parse_newline = any::<_, extra::Err<Rich<char>>>()
        .filter(|c: &char| *c == '\n' || *c == '\r' || c.is_separator_line() || c.is_separator_paragraph());

    let newline = parse_newline
        .to(NorgToken::SingleNewline);

    let newlines = parse_newline
        .repeated()
        .at_least(2)
        .collect::<String>()
        .map(|s| NorgToken::Newlines(s.len() as u16));

    let special = one_of(SPECIAL_CHARS).map(NorgToken::Special);

    let escape = just('\\').ignore_then(any()).map(NorgToken::Escape);

    let tag_end = one_of(TAG_CHARS)
        .then_ignore(keyword("end"))
        .then_ignore(choice((one_of("\n\r").rewind().map(|_| ()), end())))
        .map(NorgToken::End);

    let tokens = choice((tag_end, escape, special, newlines, newline, ws, character))
        .repeated()
        .collect::<Vec<_>>();

    tokens
        .then(end().to(NorgToken::Eof))
        .map(|(mut v, eof)| { v.push(eof); v })
}