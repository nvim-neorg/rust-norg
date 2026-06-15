//! Converts a set of Norg tokens into a set of blocks.

use std::fmt::Write as _;

use chumsky::prelude::*;
use itertools::Itertools;
use serde::Serialize;

use crate::stage_1::NorgToken;

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
            },
            Some(NorgToken::End(x)) => Some(ParagraphSegmentToken::Text(format!("{x}end"))),
            Some(NorgToken::Newlines(_)) => Some(ParagraphSegmentToken::Whitespace),
            Some(NorgToken::Eof) => Some(ParagraphSegmentToken::Text(String::new())),
            None => None,
            _x => {
                unreachable!();
            }
        })
        .collect()
}

/// Represents various Norg blocks parsed from tokens.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize)]
pub enum NorgBlock {
    ParagraphSegment(ParagraphTokenList),
    ParagraphSegmentEnd(ParagraphTokenList),
    Heading {
        level: u16,
        title: ParagraphTokenList,
        extension_section: ParagraphTokenList,
    },
    NestableDetachedModifier {
        modifier_type: char,
        level: u16,
        extension_section: ParagraphTokenList,
    },
    RangeableDetachedModifier {
        ranged: bool,
        modifier_type: char,
        title: ParagraphTokenList,
        extension_section: ParagraphTokenList,
    },
    RangeableDetachedModifierClose(char),
    RangedTag {
        tag_type: char,
        name: ParagraphTokenList,
        parameters: Option<Vec<ParagraphTokenList>>,
    },
    RangedTagEnd(char),
    VerbatimRangedTag {
        name: ParagraphTokenList,
        parameters: Option<Vec<ParagraphTokenList>>,
        content: Vec<NorgToken>,
    },
    InfirmTag {
        name: ParagraphTokenList,
        parameters: Option<Vec<ParagraphTokenList>>,
    },
    CarryoverTag {
        tag_type: char,
        name: ParagraphTokenList,
        parameters: Option<Vec<ParagraphTokenList>>,
    },
    DelimitingModifier(char),
}

/// Defines the parser for stage 2 of the Norg parsing process, which converts tokens into blocks.
pub fn stage_2<'src>() -> impl Parser<'src, &'src [NorgToken], Vec<NorgBlock>, extra::Err<Rich<'src, NorgToken>>> {
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

    let not_newlines_or_eof = any()
        .filter(|tok: &NorgToken| !matches!(tok, NorgToken::SingleNewline | NorgToken::Newlines(_) | NorgToken::Eof));

    let not_newlines_ws_or_eof = any()
        .filter(|tok: &NorgToken| !matches!(tok, NorgToken::Newlines(_) | NorgToken::SingleNewline | NorgToken::Whitespace(_) | NorgToken::Eof));

    let not_newlines_ws_or_eof_or_close_paren = any()
        .filter(|tok: &NorgToken| !matches!(tok, NorgToken::SingleNewline | NorgToken::Newlines(_) | NorgToken::Eof | NorgToken::Special(')')));

    let paragraph_segment = not_newlines_or_eof
        .repeated()
        .at_least(1)
        .collect::<Vec<_>>();

    let extension_section = not_newlines_ws_or_eof_or_close_paren
        .repeated()
        .at_least(1)
        .collect::<Vec<_>>()
        .delimited_by(just(Special('(')), just(Special(')')));

    let parameters = not_newlines_ws_or_eof
        .repeated()
        .at_least(1)
        .collect::<Vec<_>>()
        .separated_by(whitespace.repeated().at_least(1).collect::<Vec<_>>())
        .collect::<Vec<_>>();

    let heading = select! {
        Special('*') => (),
    }
    .repeated()
    .at_least(1)
    .collect::<Vec<_>>()
    .map(|chars| chars.len() as u16)
    .then_ignore(whitespace.repeated().at_least(1).collect::<Vec<_>>())
    .then(extension_section.clone().or_not())
    .then(paragraph_segment.clone())
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
    .collect::<Vec<_>>()
    .try_map(|chars, span| {
        if chars.iter().all_equal() {
            Ok((chars[0], chars.len() as u16))
        } else {
            let modifier_type = match chars[0] {
                '-' => "unordered list",
                '~' => "ordered list",
                '>' => "quote",
                _ => unreachable!(),
            };
            Err(Rich::custom(
                span,
                format!(
                    "
                    Expected a sequence of '{}' characters when creating {}.
                    Norg does not permit mixing of modifiers, e.g. `-~>`. Keep all your modifiers the same, e.g. `---`.
                ",
                    chars[0], modifier_type
                ),
            ))
        }
    })
    .then_ignore(whitespace.repeated().at_least(1).collect::<Vec<_>>())
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
            .collect::<Vec<_>>()
            .map(|chars| (chars[0], chars.len() == 2))
            .then_ignore(whitespace.repeated().at_least(1).collect::<Vec<_>>())
            .then(extension_section.clone().or_not())
            .then(paragraph_segment.clone())
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

        let not_tag_end_or_ws = any()
            .filter(move |tok: &NorgToken| !matches!(tok, NorgToken::Newlines(_) | NorgToken::SingleNewline | NorgToken::Whitespace(_) | NorgToken::Eof | NorgToken::End(_) if matches!(tok, NorgToken::End(x) if *x == c)));

        let tag_parameters = not_tag_end_or_ws
            .clone()
            .repeated()
            .at_least(1)
            .collect::<Vec<_>>()
            .separated_by(whitespace.repeated().at_least(1).collect::<Vec<_>>())
            .collect::<Vec<_>>();

        let verbatim_content = any()
            .filter(move |tok: &NorgToken| !matches!(tok, NorgToken::End(ref x) if *x == c))
            .repeated()
            .collect::<Vec<_>>()
            .or_not();

        parse_char
            .ignore_then(not_newlines_ws_or_eof.repeated().at_least(1).collect::<Vec<_>>())
            .then(
                whitespace
                    .repeated()
                    .at_least(1)
                    .collect::<Vec<_>>()
                    .ignore_then(tag_parameters)
                    .or_not(),
            )
            .then_ignore(just(SingleNewline).or_not())
            .then_ignore(any().filter(|tok: &NorgToken| matches!(tok, Newlines(_))).or_not())
            .then(verbatim_content)
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
                    content: content.unwrap_or_default(),
                },
            )
    };

    let ranged_tag = |c: char| {
        let parse_char = select! { Special(x) if x == c => x };

        parse_char
            .ignore_then(not_newlines_ws_or_eof.repeated().at_least(1).collect::<Vec<_>>())
            .then(
                whitespace
                    .repeated()
                    .at_least(1)
                    .collect::<Vec<_>>()
                    .ignore_then(parameters.clone())
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
            .ignore_then(not_newlines_ws_or_eof.repeated().at_least(1).collect::<Vec<_>>())
            .then(
                whitespace
                    .repeated()
                    .at_least(1)
                    .collect::<Vec<_>>()
                    .ignore_then(parameters.clone())
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
        .then(not_newlines_ws_or_eof.repeated().at_least(1).collect::<Vec<_>>())
        .then(
            whitespace
                .repeated()
                .at_least(1)
                .collect::<Vec<_>>()
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

    let delimiting_mod = select! {
        NorgToken::Special(c @ ('-' | '=' | '_')) => c,
    }
    .repeated()
    .at_least(2)
    .collect::<Vec<_>>()
    .then_ignore(newlines_or_eof)
    .map(|chars| NorgBlock::DelimitingModifier(chars[0]));

    choice((
        heading,
        nestable_detached_modifier,
        delimiting_mod,
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
            .clone()
            .then(newlines_or_eof.repeated().at_least(1).collect::<Vec<_>>().rewind())
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
    .collect::<Vec<_>>()
    .then_ignore(just(Eof))
}