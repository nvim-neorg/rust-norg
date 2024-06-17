//! This file contains the initial lexing stage, which breaks up characters into distinct tokens.

use chumsky::prelude::*;
use chumsky::{
    text::{keyword, Character},
    Parser,
};
use serde::Serialize;

/// Describes an individual part of the document.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize)]
pub enum NorgToken {
    Whitespace,
    SingleNewline,
    Newlines,
    Text(String),
    Special(char),
    Escape(char),
    End(char),
    Eof,
}

/// A list of characters which are considered "special", i.e. for parsing of attached modifiers.
const SPECIAL_CHARS: &str = "*-~/_!%^,\"'$:@|=.#+<>()[]{}";

/// Parses a `.norg` document and breaks it up into tokens.
pub fn stage_1() -> impl Parser<char, Vec<NorgToken>, Error = chumsky::error::Simple<char>> {
    // FIXME(vhyrro): `is_inline_whitespace` does not, in any way, respect unicode whitespace characters.
    let ws = filter(|c: &char| c.is_inline_whitespace())
        .repeated()
        .at_least(1)
        .to(NorgToken::Whitespace);

    let word = filter(|c: &char| c.is_whitespace() || *c == '\\')
        .or(one_of(SPECIAL_CHARS))
        .not()
        .repeated()
        .at_least(1)
        .collect()
        .map(NorgToken::Text);

    let newline = one_of("\n\r")
        .then_ignore(ws.clone().repeated())
        .to(NorgToken::SingleNewline);

    let newlines = one_of("\n\r")
        .repeated()
        .at_least(2)
        .then_ignore(ws.clone().repeated())
        .to(NorgToken::Newlines);

    let special = one_of(SPECIAL_CHARS).map(NorgToken::Special);

    let escape = just('\\').ignore_then(any()).map(NorgToken::Escape);

    let tag_end = one_of(SPECIAL_CHARS)
        .then_ignore(keyword("end"))
        .then_ignore(one_of("\n\r").rewind())
        .map(NorgToken::End);

    choice((tag_end, escape, special, word, ws, newlines, newline))
        .repeated()
        .chain(end().to(NorgToken::Eof))
}
