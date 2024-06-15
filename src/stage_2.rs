//! Converts a set of Norg tokens into a set of blocks.

use chumsky::Parser;
use itertools::Itertools;
use serde::Serialize;

use crate::stage_1::NorgToken;
use chumsky::prelude::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize)]
pub enum NorgBlock {
    ParagraphSegment(Vec<NorgToken>),
    ParagraphSegmentEnd(Vec<NorgToken>),
    Heading {
        level: u16,
        title: Vec<NorgToken>,
        extension_section: Vec<NorgToken>,
    },
    NestableDetachedModifier {
        modifier_type: char,
        level: u16,
        extension_section: Vec<NorgToken>,
    },
    RangeableDetachedModifier {
        ranged: bool,
        modifier_type: char,
        title: Vec<NorgToken>,
        extension_section: Vec<NorgToken>,
    },
    RangeableDetachedModifierClose(char),
    RangedTag {
        tag_type: char,
        name: Vec<NorgToken>,
        parameters: Option<Vec<Vec<NorgToken>>>,
    },
    RangedTagEnd(char),
    VerbatimRangedTag {
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

pub fn stage_2() -> impl Parser<NorgToken, Vec<NorgBlock>, Error = chumsky::error::Simple<NorgToken>>
{
    use NorgToken::*;

    let paragraph_segment = none_of([SingleNewline, Newlines, Eof])
        .repeated()
        .at_least(1);

    let extension_section = none_of([Special(')'), Newlines, SingleNewline, Eof])
        .repeated()
        .at_least(1)
        .delimited_by(just(Special('(')), just(Special(')')));

    let parameters = none_of([Newlines, SingleNewline, Eof, Whitespace])
        .repeated()
        .at_least(1)
        .separated_by(just(Whitespace).repeated().at_least(1));

    let heading = select! {
        Special('*') => '*',
    }
    .repeated()
    .at_least(1)
    .map(|chars| chars.len() as u16)
    .then_ignore(just(Whitespace).repeated().at_least(1))
    .then(extension_section.clone().or_not())
    .then(paragraph_segment.clone())
    .then_ignore(one_of([SingleNewline, Newlines, Eof]))
    .map(|((level, extension_section), title)| NorgBlock::Heading {
        level,
        title,
        extension_section: extension_section.unwrap_or_default(),
    })
    .labelled("heading");

    let nestable_detached_modifier = select! {
        Special(c) if c == '-' || c == '~' || c == '>' => c,
    }
    .repeated()
    .at_least(1)
    .try_map(|chars, span| {
        if chars.iter().all_equal() {
            Ok((chars[0], chars.len() as u16))
        } else {
            // Get the type of element that the user tried to create.
            let modifier_type = match chars[0] {
                '-' => "unordered list",
                '~' => "ordered list",
                '>' => "quote",
                _ => unreachable!(),
            };
            Err(Simple::custom(
                span,
                format!("
                    Expected a sequence of '{}' characters when creating {}.
                    Norg does not permit mixing of modifiers, e.g. `-~>`. Keep all your modifiers the same, e.g. `---`.
                ", chars[0], modifier_type),
            ))
        }
    })
    .then_ignore(just(Whitespace).repeated().at_least(1))
    .then(extension_section.clone().or_not())
    .map(
        |((modifier_type, level), extension_section)| NorgBlock::NestableDetachedModifier {
            modifier_type,
            level,
            extension_section: extension_section.unwrap_or_default(),
        },
    )
    .labelled("nestabled_detached_modifier");

    let rangeable_mod = |c: char| {
        select! { Special(x) if x == c => x }
            .repeated()
            .at_least(1)
            .at_most(2)
            .map(|chars| (chars[0], chars.len() == 2))
            .then_ignore(just(Whitespace).repeated().at_least(1))
            .then(extension_section.clone().or_not())
            .then(paragraph_segment.clone())
            .then_ignore(one_of([SingleNewline, Newlines, Eof]))
            .map(|(((modifier_type, ranged), extension_section), title)| {
                NorgBlock::RangeableDetachedModifier {
                    modifier_type,
                    ranged,
                    title,
                    extension_section: extension_section.unwrap_or_default(),
                }
            })
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
                    .ignore_then(parameters.clone())
                    .or_not(),
            )
            .then_ignore(one_of([SingleNewline, Newlines]))
            .then(tag_end.not().repeated())
            .then_ignore(tag_end)
            .map(
                |((name, parameters), content)| NorgBlock::VerbatimRangedTag {
                    name,
                    parameters,
                    content,
                },
            )
    };

    let ranged_tag = |c: char| {
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
                    .ignore_then(parameters.clone())
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
                    .ignore_then(parameters.clone())
                    .or_not(),
            )
            .then_ignore(one_of([SingleNewline, Newlines]))
            .map(|(name, parameters)| NorgBlock::InfirmTag { name, parameters })
    };

    let carryover_tags = {
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
                .ignore_then(parameters.clone())
                .or_not(),
        )
        .then_ignore(one_of([SingleNewline, Newlines]))
        .map(|((tag_type, name), parameters)| NorgBlock::CarryoverTag {
            tag_type,
            name,
            parameters,
        })
    };

    let tag_end = select! {
        NorgToken::End(c) => NorgBlock::RangedTagEnd(c),
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
        tag_end,
        paragraph_segment
            .then(
                one_of([SingleNewline, Newlines, Eof])
                    .repeated()
                    .at_least(1)
                    .rewind(),
            )
            .map(|(content, trailing)| match trailing.last().unwrap() {
                Eof | Newlines => NorgBlock::ParagraphSegmentEnd(content),
                SingleNewline => NorgBlock::ParagraphSegment(content),
                _ => unreachable!(),
            })
            .labelled("paragraph_segment"),
    ))
    .padded_by(one_of([Whitespace, SingleNewline, Newlines]).repeated())
    .repeated()
    .then_ignore(just(Eof))
}
