use chumsky::prelude::*;
use chumsky::text::{keyword, Character};
use chumsky::Parser;
use clap::Parser as ClapParser;
use eyre::Result;
use std::path::PathBuf;

#[derive(ClapParser)]
/// A compiler for the ef language.
struct Norg {
    /// The file to compile.
    file: PathBuf,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum NorgToken {
    Whitespace,
    SingleNewline,
    Newlines,
    Text(String),
    Special(char),
    Escape(char),
    End(char),
    Eof,
}

fn lexer() -> impl Parser<char, Vec<NorgToken>, Error = chumsky::error::Simple<char>> {
    let special_chars = "*-~/_!%^,\"'$:@|=.#+>";

    let ws = filter(|c: &char| c.is_inline_whitespace())
        .repeated()
        .at_least(1)
        .to(NorgToken::Whitespace);
    let word = filter(|c: &char| c.is_whitespace() || *c == '\\')
        .or(one_of(special_chars))
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

    let special = one_of(special_chars).map(NorgToken::Special);

    let escape = just('\\').ignore_then(any()).map(NorgToken::Escape);

    let tag_end = one_of(special_chars)
        .then_ignore(keyword("end"))
        .then_ignore(one_of("\n\r").rewind())
        .map(NorgToken::End);

    choice((tag_end, escape, special, word, ws, newlines, newline))
        .repeated()
        .chain(end().to(NorgToken::Eof))
}

#[derive(Debug, Clone, PartialEq)]
enum NorgBlock {
    ParagraphSegment(Vec<NorgToken>),
    Heading {
        level: u16,
        title: Vec<NorgToken>,
    },
    NestableDetachedModifier {
        modifier_type: char,
        level: u16,
    },
    RangeableDetachedModifier {
        ranged: bool,
        modifier_type: char,
        title: Vec<NorgToken>,
    },
    RangeableDetachedModifierClose(char),
    RangedTag {
        tag_type: char,
        name: Vec<NorgToken>,
        parameters: Option<Vec<Vec<NorgToken>>>,
    },
    VerbatimRangedTag {
        tag_type: char,
        name: Vec<NorgToken>,
        parameters: Option<Vec<Vec<NorgToken>>>,
        content: Vec<NorgToken>,
    },
    InfirmTag {
        name: Vec<NorgToken>,
        parameters: Option<Vec<Vec<NorgToken>>>,
    },
    CarryoverTag {
        tag_type: char,
        name: Vec<NorgToken>,
        parameters: Option<Vec<Vec<NorgToken>>>,
    },
}

fn block_level() -> impl Parser<NorgToken, Vec<NorgBlock>, Error = chumsky::error::Simple<NorgToken>>
{
    use NorgToken::*;

    let paragraph_segment = none_of([SingleNewline, Newlines, Eof])
        .repeated()
        .at_least(1);

    let heading = select! {
        Special('*') => '*',
    }
    .repeated()
    .at_least(1)
    .map(|chars| chars.len() as u16)
    .then_ignore(just(Whitespace).repeated().at_least(1))
    .then(paragraph_segment.clone())
    .then_ignore(one_of([SingleNewline, Newlines, Eof]))
    .map(|(level, content)| NorgBlock::Heading {
        level,
        title: content,
    })
    .labelled("heading");

    let nestable_detached_modifier = select! {
        Special(c) if c == '-' || c == '~' || c == '>' => c,
    }
    .repeated()
    .at_least(1)
    // TODO: Validate the tree by ensuring all chars are the same.
    .map(|chars| (chars[0], chars.len() as u16))
    .then_ignore(just(Whitespace).repeated().at_least(1))
    .map(|(modifier_type, level)| NorgBlock::NestableDetachedModifier { modifier_type, level })
    .labelled("nestabled_detached_modifier");

    let rangeable_mod = |c: char| {
        select! { Special(x) if x == c => x }
            .repeated()
            .at_least(1)
            .at_most(2)
            .map(|chars| (chars[0], chars.len() == 2))
            .then_ignore(just(Whitespace).repeated().at_least(1))
            .then(paragraph_segment.clone())
            .then_ignore(one_of([SingleNewline, Newlines, Eof]))
            .map(
                |((modifier_type, ranged), title)| NorgBlock::RangeableDetachedModifier {
                    modifier_type,
                    ranged,
                    title,
                },
            )
            .labelled("rangeable_detached_modifier")
    };

    let rangeable_mod_closer = |c: char| {
        select! { Special(x) if x == c => x }
            .repeated()
            .exactly(2)
            .ignored()
            .then_ignore(one_of([SingleNewline, Newlines, Eof]))
            .map(move |_| NorgBlock::RangeableDetachedModifierClose(c))
            .labelled("rangeable_detached_modifier_closed")
    };

    let verbatim_ranged_tag = |c: char| {
        let parameters = none_of([Newlines, SingleNewline, Eof, Whitespace])
            .repeated()
            .at_least(1)
            .separated_by(just(Whitespace).repeated().at_least(1));

        let parse_char = select! { Special(x) if x == c => x };
        let tag_end = select! {
                End(x) if x == c => x,
        };

        parse_char
            .ignore_then(
                none_of([Newlines, SingleNewline, Eof, Whitespace])
                    .repeated()
                    .at_least(1),
            )
            .then(
                just(Whitespace)
                    .repeated()
                    .at_least(1)
                    .ignore_then(parameters)
                    .or_not(),
            )
            .then_ignore(one_of([SingleNewline, Newlines]))
            .then(tag_end.not().repeated())
            .then_ignore(tag_end)
            .map(
                move |((name, parameters), content)| NorgBlock::VerbatimRangedTag {
                    tag_type: c,
                    name,
                    parameters,
                    content,
                },
            )
    };

    let ranged_tag = |c: char| {
        let parameters = none_of([Newlines, SingleNewline, Eof, Whitespace])
            .repeated()
            .at_least(1)
            .separated_by(just(Whitespace).repeated().at_least(1));

        let parse_char = select! { Special(x) if x == c => x };

        parse_char
            .ignore_then(
                none_of([Newlines, SingleNewline, Eof, Whitespace])
                    .repeated()
                    .at_least(1),
            )
            .then(
                just(Whitespace)
                    .repeated()
                    .at_least(1)
                    .ignore_then(parameters)
                    .or_not(),
            )
            .then_ignore(one_of([SingleNewline, Newlines]))
            .map(move |(name, parameters)| NorgBlock::RangedTag {
                tag_type: c,
                name,
                parameters,
            })
    };

    let infirm_tag = {
        let parameters = none_of([Newlines, SingleNewline, Eof, Whitespace])
            .repeated()
            .at_least(1)
            .separated_by(just(Whitespace).repeated().at_least(1));

        select! { Special('.') => '.' }
            .ignore_then(
                none_of([Newlines, SingleNewline, Eof, Whitespace])
                    .repeated()
                    .at_least(1),
            )
            .then(
                just(Whitespace)
                    .repeated()
                    .at_least(1)
                    .ignore_then(parameters)
                    .or_not(),
            )
            .then_ignore(one_of([SingleNewline, Newlines]))
            .map(|(name, parameters)| NorgBlock::InfirmTag { name, parameters })
    };

    let carryover_tags = {
        let parameters = none_of([Newlines, SingleNewline, Eof, Whitespace])
            .repeated()
            .at_least(1)
            .separated_by(just(Whitespace).repeated().at_least(1));

        select! {
            Special('+') => '+',
            Special('#') => '#',
        }
        .then(
            none_of([Newlines, SingleNewline, Eof, Whitespace])
                .repeated()
                .at_least(1),
        )
        .then(
            just(Whitespace)
                .repeated()
                .at_least(1)
                .ignore_then(parameters)
                .or_not(),
        )
        .then_ignore(one_of([SingleNewline, Newlines]))
        .map(|((tag_type, name), parameters)| NorgBlock::CarryoverTag {
            tag_type,
            name,
            parameters,
        })
    };

    choice((
        heading,
        nestable_detached_modifier,
        rangeable_mod('$'),
        rangeable_mod_closer('$'),
        rangeable_mod('^'),
        rangeable_mod_closer('^'),
        rangeable_mod(':'),
        rangeable_mod_closer(':'),
        verbatim_ranged_tag('@'),
        ranged_tag('|'),
        ranged_tag('='),
        infirm_tag,
        carryover_tags,
        paragraph_segment
            .chain(one_of([SingleNewline, Newlines, Eof]))
            .map(NorgBlock::ParagraphSegment)
            .labelled("paragraph_segment"),
    ))
    .padded_by(one_of([Whitespace, SingleNewline, Newlines]).repeated())
    .repeated()
    .then_ignore(just(Eof))
}

// enum NorgASTFlat {}

fn main() -> Result<()> {
    let parser = Norg::parse();

    let content = String::from_utf8(std::fs::read(parser.file)?)?;

    println!(
        "{:#?}",
        block_level().parse(lexer().parse_recovery(content).0.expect("Failed"))
    );

    Ok(())
}
