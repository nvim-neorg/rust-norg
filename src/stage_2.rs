//! Converts a set of Norg tokens into a set of blocks.

use std::fmt::Write as _;

use chumsky::Parser;
use itertools::Itertools;
use serde::Serialize;

use crate::stage_1::NorgToken;
use chumsky::prelude::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize)]
pub enum ParagraphSegmentToken {
    Text(String),
    Whitespace,
    Special(char),
    Escape(char),
}

impl std::fmt::Display for ParagraphSegmentToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Escape(c) => write!(f, "\\{}", c),
            Self::Text(str) => f.write_str(str),
            Self::Special(c) => f.write_char(*c),
            Self::Whitespace => f.write_char(' '),
        }
    }
}

impl From<ParagraphSegmentToken> for String {
    fn from(value: ParagraphSegmentToken) -> Self {
        value.to_string()
    }
}

pub type ParagraphTokenList = Vec<ParagraphSegmentToken>;

fn tokens_to_paragraph_segment(tokens: Vec<NorgToken>) -> ParagraphTokenList {
    tokens
        .into_iter()
        .peekable()
        .batching(|it| match it.next() {
            Some(NorgToken::SingleNewline) | Some(NorgToken::Whitespace(_)) => {
                Some(ParagraphSegmentToken::Whitespace)
            }
            Some(NorgToken::Special(c)) => Some(ParagraphSegmentToken::Special(c)),
            Some(NorgToken::Escape(c)) => Some(ParagraphSegmentToken::Escape(c)),
            Some(NorgToken::Regular(c)) => {
                let mut result: String = it
                    .peeking_take_while(|token| matches!(token, NorgToken::Regular(_)))
                    .map_into::<String>()
                    .collect();

                result.insert(0, c);

                Some(ParagraphSegmentToken::Text(result))
            }
            None => None,
            _ => unreachable!(),
        })
        .collect()
}

/// Represents various Norg blocks parsed from tokens.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize)]
pub enum NorgBlock {
    /// A segment of a paragraph consisting of Norg tokens.
    ParagraphSegment(ParagraphTokenList),
    /// End of a paragraph segment.
    ParagraphSegmentEnd(ParagraphTokenList),
    /// A heading with a specified level, title, and optional extension section.
    Heading {
        level: u16,
        title: ParagraphTokenList,
        extension_section: ParagraphTokenList,
    },
    /// A nestable detached modifier with a type, level, and optional extension section.
    NestableDetachedModifier {
        modifier_type: char,
        level: u16,
        extension_section: ParagraphTokenList,
    },
    /// A rangeable detached modifier with an indication if it is ranged, type, title, and optional extension section.
    RangeableDetachedModifier {
        ranged: bool,
        modifier_type: char,
        title: ParagraphTokenList,
        extension_section: ParagraphTokenList,
    },
    /// Closing tag for a rangeable detached modifier.
    RangeableDetachedModifierClose(char),
    /// A ranged tag with a type, name, and optional parameters.
    RangedTag {
        tag_type: char,
        name: ParagraphTokenList,
        parameters: Option<Vec<ParagraphTokenList>>,
    },
    /// End of a ranged tag.
    RangedTagEnd(char),
    /// A verbatim ranged tag with a name, optional parameters, and content.
    VerbatimRangedTag {
        name: ParagraphTokenList,
        parameters: Option<Vec<ParagraphTokenList>>,
        content: Vec<NorgToken>,
    },
    /// An infirm tag with a name and optional parameters.
    InfirmTag {
        name: ParagraphTokenList,
        parameters: Option<Vec<ParagraphTokenList>>,
    },
    /// A carryover tag with a type, name, and optional parameters.
    CarryoverTag {
        tag_type: char,
        name: ParagraphTokenList,
        parameters: Option<Vec<ParagraphTokenList>>,
    },
}

/// Defines the parser for stage 2 of the Norg parsing process, which converts tokens into blocks.
///
/// # Returns
///
/// * A parser that processes `NorgToken`s into a vector of `NorgBlock`s, which properly define
///   paragraph boundaries.
pub fn stage_2() -> impl Parser<NorgToken, Vec<NorgBlock>, Error = chumsky::error::Simple<NorgToken>>
{
    use NorgToken::*;

    let whitespace = select! { Whitespace(_) => () };

    let newlines_or_eof = select! {
        s @ SingleNewline => s,
        n @ Newlines(..) => n,
        e @ Eof => e,
    };

    let newlines_whitespace = select! {
        Newlines(_) => (),
        SingleNewline => (),
        Whitespace(_) => (),
    };

    let newlines_whitespace_or_eof = select! {
        Newlines(_) => (),
        SingleNewline => (),
        Whitespace(_) => (),
        Eof => (),
    };

    let paragraph_segment = newlines_or_eof.not().repeated().at_least(1);

    let extension_section = select! {
        SingleNewline => (),
        Newlines(_) => (),
        Eof => (),
        Special(')') => (),
    }
    .not()
    .repeated()
    .at_least(1)
    .delimited_by(just(Special('(')), just(Special(')')));

    let parameters = newlines_whitespace_or_eof
        .not()
        .repeated()
        .at_least(1)
        .separated_by(whitespace.repeated().at_least(1));

    let heading = select! {
        Special('*') => (),
    }
    .ignored()
    .repeated()
    .at_least(1)
    .map(|chars| chars.len() as u16)
    .then_ignore(whitespace.repeated().at_least(1))
    .then(extension_section.clone().or_not())
    .then(paragraph_segment)
    .then_ignore(newlines_or_eof)
    .map(|((level, extension_section), title)| NorgBlock::Heading {
        level,
        title: tokens_to_paragraph_segment(title),
        extension_section: extension_section
            .map(tokens_to_paragraph_segment)
            .unwrap_or_default(),
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
    .then_ignore(whitespace.repeated().at_least(1))
    .then(extension_section.clone().or_not())
    .map(
        |((modifier_type, level), extension_section)| NorgBlock::NestableDetachedModifier {
            modifier_type,
            level,
            extension_section: extension_section.map(tokens_to_paragraph_segment).unwrap_or_default(),
        },
    )
    .labelled("nestabled_detached_modifier");

    let rangeable_mod = |c: char| {
        select! { Special(x) if x == c => x }
            .repeated()
            .at_least(1)
            .at_most(2)
            .map(|chars| (chars[0], chars.len() == 2))
            .then_ignore(whitespace.repeated().at_least(1))
            .then(extension_section.clone().or_not())
            .then(paragraph_segment)
            .then_ignore(newlines_or_eof)
            .map(|(((modifier_type, ranged), extension_section), title)| {
                NorgBlock::RangeableDetachedModifier {
                    modifier_type,
                    ranged,
                    title: tokens_to_paragraph_segment(title),
                    extension_section: extension_section
                        .map(tokens_to_paragraph_segment)
                        .unwrap_or_default(),
                }
            })
            .labelled("rangeable_detached_modifier")
    };

    let rangeable_mod_closer = |c: char| {
        select! { Special(x) if x == c => x }
            .repeated()
            .exactly(2)
            .ignored()
            .then_ignore(newlines_or_eof)
            .map(move |_| NorgBlock::RangeableDetachedModifierClose(c))
            .labelled("rangeable_detached_modifier_closed")
    };

    let verbatim_ranged_tag = |c: char| {
        let parse_char = select! { Special(x) if x == c => x };
        let tag_end = select! {
                End(x) if x == c => x,
        };

        parse_char
            .ignore_then(newlines_whitespace_or_eof.not().repeated().at_least(1))
            .then(
                whitespace
                    .repeated()
                    .at_least(1)
                    .ignore_then(parameters)
                    .or_not(),
            )
            .then_ignore(select! {
                SingleNewline => (),
                Newlines(_) => (),
            })
            .then(tag_end.not().repeated())
            .then_ignore(tag_end)
            .map(
                |((name, parameters), content)| NorgBlock::VerbatimRangedTag {
                    name: tokens_to_paragraph_segment(name),
                    parameters: parameters.map(|tokens| {
                        tokens
                            .into_iter()
                            .map(tokens_to_paragraph_segment)
                            .collect()
                    }),
                    content,
                },
            )
    };

    let ranged_tag = |c: char| {
        let parse_char = select! { Special(x) if x == c => x };

        parse_char
            .ignore_then(newlines_whitespace_or_eof.not().repeated().at_least(1))
            .then(
                whitespace
                    .repeated()
                    .at_least(1)
                    .ignore_then(parameters)
                    .or_not(),
            )
            .then_ignore(select! {
                SingleNewline => (),
                Newlines(_) => (),
            })
            .map(move |(name, parameters)| NorgBlock::RangedTag {
                tag_type: c,
                name: tokens_to_paragraph_segment(name),
                parameters: parameters.map(|tokens| {
                    tokens
                        .into_iter()
                        .map(tokens_to_paragraph_segment)
                        .collect()
                }),
            })
    };

    let infirm_tag = {
        select! { Special('.') => '.' }
            .ignore_then(newlines_whitespace_or_eof.not().repeated().at_least(1))
            .then(
                whitespace
                    .repeated()
                    .at_least(1)
                    .ignore_then(parameters)
                    .or_not(),
            )
            .then_ignore(select! {
                SingleNewline => (),
                Newlines(_) => (),
            })
            .map(|(name, parameters)| NorgBlock::InfirmTag {
                name: tokens_to_paragraph_segment(name),
                parameters: parameters.map(|tokens| {
                    tokens
                        .into_iter()
                        .map(tokens_to_paragraph_segment)
                        .collect()
                }),
            })
    };

    let carryover_tags = {
        select! {
            Special('+') => '+',
            Special('#') => '#',
        }
        .then(newlines_whitespace_or_eof.not().repeated().at_least(1))
        .then(
            whitespace
                .repeated()
                .at_least(1)
                .ignore_then(parameters)
                .or_not(),
        )
        .then_ignore(select! {
        Newlines(_) => (),
        SingleNewline => (),
        })
        .map(|((tag_type, name), parameters)| NorgBlock::CarryoverTag {
            tag_type,
            name: tokens_to_paragraph_segment(name),
            parameters: parameters.map(|tokens| {
                tokens
                    .into_iter()
                    .map(tokens_to_paragraph_segment)
                    .collect()
            }),
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
            .then(newlines_or_eof.repeated().at_least(1).rewind())
            .map(|(content, trailing)| match trailing.last().unwrap() {
                NorgToken::Eof => {
                    NorgBlock::ParagraphSegmentEnd(tokens_to_paragraph_segment(content))
                }
                NorgToken::Newlines(_) => {
                    NorgBlock::ParagraphSegmentEnd(tokens_to_paragraph_segment(content))
                }
                NorgToken::SingleNewline => NorgBlock::ParagraphSegment(
                    tokens_to_paragraph_segment(content.into_iter().chain(trailing).collect()),
                ),
                _ => unreachable!(),
            })
            .labelled("paragraph_segment"),
    ))
    .padded_by(newlines_whitespace.repeated())
    .repeated()
    .then_ignore(just(Eof))
}
