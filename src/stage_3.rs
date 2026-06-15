use std::fmt::Write;

use chumsky::prelude::*;
use itertools::Itertools;
use serde::Serialize;
use textwrap::dedent;

use crate::stage_2::{NorgBlock, ParagraphSegmentToken, ParagraphTokenList};

#[derive(Clone, Copy, Hash, Debug, PartialEq, Eq, Serialize)]
pub enum NestableDetachedModifier {
    Quote,
    UnorderedList,
    OrderedList,
}

impl std::fmt::Display for NestableDetachedModifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Quote => f.write_char('>'),
            Self::UnorderedList => f.write_char('-'),
            Self::OrderedList => f.write_char('~'),
        }
    }
}

#[derive(Clone, Hash, Debug, PartialEq, Eq, Serialize)]
pub enum RangeableDetachedModifier {
    Definition,
    Footnote,
    Table,
}

impl std::fmt::Display for RangeableDetachedModifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Definition => f.write_char('$'),
            Self::Footnote => f.write_char('^'),
            Self::Table => f.write_char(':'),
        }
    }
}

#[derive(Clone, Hash, Debug, PartialEq, Eq, Serialize)]
pub enum TodoStatus {
    Undone,
    Done,
    NeedsClarification,
    Paused,
    Urgent,
    Recurring(Option<String>),
    Pending,
    Canceled,
}

#[derive(Clone, Hash, Debug, PartialEq, Eq, Serialize)]
pub enum DetachedModifierExtension {
    Todo(TodoStatus),
    Priority(String),
    Timestamp(String),
    DueDate(String),
    StartDate(String),
}

#[derive(Clone, Hash, Debug, PartialEq, Eq, Serialize)]
pub enum CarryoverTag {
    Attribute,
    Macro,
}

#[derive(PartialEq, Serialize)]
pub enum RangedTag {
    Macro,
    Standard,
}

fn paragraph_parser_opener_candidates_and_links<'a>() -> impl Parser<
    'a,
    &'a [ParagraphSegmentToken],
    Vec<ParagraphSegment>,
    extra::Err<Rich<'a, ParagraphSegmentToken>>,
> {
    let token = any().map(ParagraphSegment::Token);
    let modifier = select! {
        ParagraphSegmentToken::Special(c @ ('*' | '/' | '_' | '-')) => c,
    };

    let whitespace_or_special = select! {
        ParagraphSegmentToken::Whitespace => ParagraphSegmentToken::Whitespace,
        ParagraphSegmentToken::Special(c) => ParagraphSegmentToken::Special(c),
    };

    let opening_modifier_candidate = whitespace_or_special
        .then(modifier.repeated().at_least(1).collect::<Vec<_>>())
        .then(any().filter(|t: &ParagraphSegmentToken| !matches!(t, ParagraphSegmentToken::Whitespace)))
        .map(|((left, modifiers), right)| {
            ParagraphSegment::AttachedModifierOpener((Some(left), modifiers, right))
        });

    let left_empty_opening_modifier = modifier
        .repeated()
        .at_least(1)
        .collect::<Vec<_>>()
        .then(any().filter(|t: &ParagraphSegmentToken| !matches!(t, ParagraphSegmentToken::Whitespace)))
        .map(|(modifiers, right)| {
            ParagraphSegment::AttachedModifierOpener((None, modifiers, right))
        });

    let not_backtick = any()
        .filter(|t: &ParagraphSegmentToken| !matches!(t, ParagraphSegmentToken::Special('`')));

    let inline_verbatim = just(ParagraphSegmentToken::Special('`'))
        .ignore_then(not_backtick.repeated().at_least(1).collect::<Vec<_>>())
        .then_ignore(just(ParagraphSegmentToken::Special('`')))
        .map(ParagraphSegment::InlineVerbatim);

    let not_close_bracket = any()
        .filter(|t: &ParagraphSegmentToken| !matches!(t, ParagraphSegmentToken::Special(']')));

    let anchor = just(ParagraphSegmentToken::Special('['))
        .ignore_then(not_close_bracket.repeated().at_least(1).collect::<Vec<_>>())
        .then_ignore(just(ParagraphSegmentToken::Special(']')));

    let not_colon = any()
        .filter(|t: &ParagraphSegmentToken| !matches!(t, ParagraphSegmentToken::Special(':')));

    let filepath_inner = just(ParagraphSegmentToken::Special(':'))
        .ignore_then(not_colon.repeated().at_least(1).collect::<Vec<_>>())
        .then_ignore(just(ParagraphSegmentToken::Special(':')));

    let link = just(ParagraphSegmentToken::Special('{'))
        .ignore_then(filepath_inner.or_not())
        .then(
            choice((
                just(ParagraphSegmentToken::Special('*'))
                    .repeated()
                    .at_least(1)
                    .collect::<Vec<_>>()
                    .map(|tokens| "*".repeat(tokens.len())),
                just(ParagraphSegmentToken::Special('$')).to("$".to_string()),
                just(ParagraphSegmentToken::Special('^')).to("^".to_string()),
                just(ParagraphSegmentToken::Special('/')).to("/".to_string()),
                just(ParagraphSegmentToken::Special('=')).to("=".to_string()),
                just(ParagraphSegmentToken::Special('?')).to("?".to_string()),
                just(ParagraphSegmentToken::Special('@')).to("@".to_string()),
            ))
            .then_ignore(
                just(ParagraphSegmentToken::Whitespace)
                    .repeated()
                    .at_least(1),
            )
            .or_not(),
        )
        .then(
            any().filter(|t: &ParagraphSegmentToken| !matches!(t, ParagraphSegmentToken::Special('}')))
                .repeated()
                .at_least(1)
                .collect::<Vec<_>>()
                .or_not(),
        )
        .then_ignore(just(ParagraphSegmentToken::Special('}')))
        .then(anchor.clone().or_not())
        .map(
            |(((filepath, modifiers), content), description)| ParagraphSegment::Link {
                filepath: filepath
                    .map(|content| content.into_iter().map_into::<String>().collect()),
                description: description.map(parse_paragraph),
                targets: if let Some(content) = content {
                    vec![if let Some(modifiers) = modifiers {
                        match modifiers.as_str() {
                            "$" => LinkTarget::Definition(parse_paragraph(content)),
                            "^" => LinkTarget::Footnote(parse_paragraph(content)),
                            "?" => LinkTarget::Wiki(parse_paragraph(content)),
                            "=" => LinkTarget::Extendable(parse_paragraph(content)),
                            "/" => {
                                LinkTarget::Path(content.into_iter().map_into::<String>().collect())
                            }
                            "@" => LinkTarget::Timestamp(
                                content.into_iter().map_into::<String>().collect(),
                            ),
                            str => LinkTarget::Heading {
                                level: str.len() as u16,
                                title: parse_paragraph(content),
                            },
                        }
                    } else {
                        LinkTarget::Url(content.into_iter().map_into::<String>().collect())
                    }]
                } else {
                    vec![]
                },
            },
        );

    let not_close_angle = any()
        .filter(|t: &ParagraphSegmentToken| !matches!(t, ParagraphSegmentToken::Special('>')));

    let inline_linkable = just(ParagraphSegmentToken::Special('<'))
        .ignore_then(not_close_angle.repeated().at_least(1).collect::<Vec<_>>())
        .then_ignore(just(ParagraphSegmentToken::Special('>')))
        .map(|content| ParagraphSegment::InlineLinkTarget(parse_paragraph(content)));

    left_empty_opening_modifier.or_not().then(
        choice((
            link.clone(),
            anchor
                .clone()
                .then(link)
                .map(|(content, link)| ParagraphSegment::AnchorDefinition {
                    content: parse_paragraph(content),
                    target: Box::new(link),
                }),
            inline_verbatim,
            anchor
                .clone()
                .then(anchor.clone().or_not())
                .map(|(content, description)| ParagraphSegment::Anchor {
                    content: parse_paragraph(content),
                    description: description.map(parse_paragraph),
                }),
            inline_linkable,
            opening_modifier_candidate,
            token,
        ))
        .repeated()
        .at_least(1)
        .collect::<Vec<_>>(),
    ).map(|(prefix, mut body)| {
        let mut result = match prefix {
            Some(p) => vec![p],
            None => vec![],
        };
        result.append(&mut body);
        result
    })
}

#[allow(clippy::result_large_err)]
fn dedup_opener_candidates(input: Vec<ParagraphSegment>) -> Vec<ParagraphSegment> {
    use ParagraphSegment::*;

    input
        .into_iter()
        .coalesce(|prev, next| match (prev.clone(), next.clone()) {
            (AttachedModifierOpener(_), AttachedModifierOpener(data)) => {
                Err((prev, AttachedModifierOpenerFail(data)))
            }
            _ => Err((prev, next)),
        })
        .collect()
}

fn paragraph_parser_closer_candidates<'a>(
) -> impl Parser<'a, &'a [ParagraphSegment], Vec<ParagraphSegment>, extra::Err<Rich<'a, ParagraphSegment>>> {
    use ParagraphSegment as PS;
    use ParagraphSegmentToken as PTST;

    let token = any();
    let modifier = select! {
        PS::Token(PTST::Special(c @ ('*' | '/' | '_' | '-'))) => c,
    };

    let whitespace_or_special = select! {
        PS::Token(PTST::Whitespace) => PTST::Whitespace,
        PS::Token(PTST::Special(c)) => PTST::Special(c),
    };

    let not_whitespace = any()
        .filter(|seg: &PS| !matches!(seg, PS::Token(PTST::Whitespace)));

    let closing_modifier_candidate = not_whitespace
        .then(modifier.repeated().at_least(1).collect::<Vec<_>>())
        .then(whitespace_or_special)
        .map(|((left, modifiers), right)| {
            PS::AttachedModifierCloserCandidate((
                Box::new(left),
                modifiers,
                Some(Box::new(PS::Token(right))),
            ))
        });

    let closing_modifier_candidate_with_eof = not_whitespace
        .then(modifier.repeated().at_least(1).collect::<Vec<_>>())
        .then_ignore(end())
        .map(|(left, modifiers)| {
            PS::AttachedModifierCloserCandidate((Box::new(left), modifiers, None))
        });

    choice((
        closing_modifier_candidate,
        closing_modifier_candidate_with_eof,
        token,
    ))
    .repeated()
    .at_least(1)
    .collect::<Vec<_>>()
}

fn unravel_candidates(input: Vec<ParagraphSegment>) -> Vec<ParagraphSegment> {
    use ParagraphSegment::*;

    input
        .into_iter()
        .fold(Vec::new(), |mut acc: Vec<ParagraphSegment>, segment| {
            match segment {
                t @ Token(_) => acc.push(t),
                AttachedModifierOpener((left, modifiers, right)) => {
                    if let Some(left) = left {
                        acc.push(Token(left));
                    }
                    acc.extend(modifiers.into_iter().map(|modifier_type| {
                        AttachedModifierCandidate {
                            modifier_type,
                            content: Vec::default(),
                            closer: None,
                        }
                    }));
                    acc.push(Token(right));
                }
                AttachedModifierCloserCandidate((left, modifiers, right)) => {
                    acc.push(*left);
                    acc.extend(modifiers.into_iter().map(AttachedModifierCloser));
                    if let Some(right) = right {
                        acc.push(*right);
                    }
                }
                AttachedModifierCloser(c) => acc.push(Token(ParagraphSegmentToken::Special(c))),
                AttachedModifierOpenerFail((left, modifiers, right)) => {
                    if let Some(left) = left {
                        acc.push(Token(left));
                    }
                    acc.extend(
                        modifiers
                            .into_iter()
                            .map(|c| Token(ParagraphSegmentToken::Special(c))),
                    );
                    acc.push(Token(right));
                }
                others => acc.push(others),
            };

            acc
        })
}

fn paragraph_rollup_candidates<'a>(
) -> impl Parser<'a, &'a [ParagraphSegment], Vec<ParagraphSegment>, extra::Err<Rich<'a, ParagraphSegment>>> {
    let candidate = select! { ParagraphSegment::AttachedModifierCloser(c) => c, };

    let not_candidate = any()
        .filter(|seg: &ParagraphSegment| !matches!(seg, ParagraphSegment::AttachedModifierCloser(_)));

    let attached_modifier = recursive::<_, _, extra::Err<Rich<'a, ParagraphSegment>>, _, _>(|attached_modifier| {
        select! {
            ParagraphSegment::AttachedModifierCandidate { modifier_type, .. } => modifier_type,
        }
        .then(attached_modifier.or(not_candidate).repeated().at_least(1).collect::<Vec<_>>())
        .then(candidate)
        .try_map(|((modifier_type, content), closer), span| {
            if modifier_type == closer {
                Ok(ParagraphSegment::AttachedModifier {
                    modifier_type,
                    content,
                })
            } else {
                Err(Rich::custom(
                    span,
                    "differing opening and closing modifiers found",
                ))
            }
        })
    });

    choice((attached_modifier, any())).repeated().at_least(1).collect::<Vec<_>>()
}

fn eliminate_invalid_candidates(input: Vec<ParagraphSegment>) -> Vec<ParagraphSegment> {
    input
        .into_iter()
        .fold(Vec::new(), |mut acc: Vec<ParagraphSegment>, segment| {
            match segment {
                ParagraphSegment::AttachedModifierCandidate {
                    modifier_type,
                    content,
                    closer,
                } => {
                    acc.push(ParagraphSegment::Token(ParagraphSegmentToken::Special(
                        modifier_type,
                    )));
                    acc.extend(content);

                    if let Some(closer) = closer {
                        acc.push(*closer);
                    }
                }
                _ => acc.push(segment),
            };

            acc
        })
}

#[derive(Clone, Hash, Debug, PartialEq, Eq, Serialize)]
pub enum LinkTarget {
    Heading {
        level: u16,
        title: Vec<ParagraphSegment>,
    },
    Footnote(Vec<ParagraphSegment>),
    Definition(Vec<ParagraphSegment>),
    Generic(Vec<ParagraphSegment>),
    Wiki(Vec<ParagraphSegment>),
    Extendable(Vec<ParagraphSegment>),
    Path(String),
    Url(String),
    Timestamp(String),
}

#[derive(Debug, Clone, PartialEq, Serialize, Hash, Eq)]
pub enum ParagraphSegment {
    Token(ParagraphSegmentToken),
    AttachedModifierOpener(
        (
            Option<ParagraphSegmentToken>,
            Vec<char>,
            ParagraphSegmentToken,
        ),
    ),
    AttachedModifierOpenerFail(
        (
            Option<ParagraphSegmentToken>,
            Vec<char>,
            ParagraphSegmentToken,
        ),
    ),
    AttachedModifierCloserCandidate(
        (
            Box<ParagraphSegment>,
            Vec<char>,
            Option<Box<ParagraphSegment>>,
        ),
    ),
    AttachedModifierCloser(char),
    AttachedModifierCandidate {
        modifier_type: char,
        content: Vec<Self>,
        closer: Option<Box<Self>>,
    },
    AttachedModifier {
        modifier_type: char,
        content: Vec<Self>,
    },
    Link {
        filepath: Option<String>,
        targets: Vec<LinkTarget>,
        description: Option<Vec<ParagraphSegment>>,
    },
    AnchorDefinition {
        content: Vec<ParagraphSegment>,
        target: Box<Self>,
    },
    Anchor {
        content: Vec<ParagraphSegment>,
        description: Option<Vec<ParagraphSegment>>,
    },
    InlineLinkTarget(Vec<ParagraphSegment>),
    InlineVerbatim(Vec<ParagraphSegmentToken>),
}

fn parse_paragraph(
    input: Vec<ParagraphSegmentToken>,
) -> Vec<ParagraphSegment> {
    let stage1_result = paragraph_parser_opener_candidates_and_links()
        .parse(&input[..])
        .into_result()
        .unwrap();
    let deduped = dedup_opener_candidates(stage1_result);
    let unraveled1 = unravel_candidates(deduped);

    let stage2_result = paragraph_parser_closer_candidates()
        .parse(&unraveled1)
        .into_result()
        .unwrap();
    let unraveled2 = unravel_candidates(stage2_result);

    let stage3_result = paragraph_rollup_candidates()
        .parse(&unraveled2)
        .into_result()
        .unwrap();

    eliminate_invalid_candidates(unravel_candidates(stage3_result))
}

#[derive(Clone, Debug, PartialEq, Hash, Eq, Serialize)]
pub enum NorgASTFlat {
    Paragraph(Vec<ParagraphSegment>),
    NestableDetachedModifier {
        modifier_type: NestableDetachedModifier,
        level: u16,
        extensions: Vec<DetachedModifierExtension>,
        content: Box<Self>,
    },
    RangeableDetachedModifier {
        modifier_type: RangeableDetachedModifier,
        title: Vec<ParagraphSegment>,
        extensions: Vec<DetachedModifierExtension>,
        content: Vec<Self>,
    },
    Heading {
        level: u16,
        title: Vec<ParagraphSegment>,
        extensions: Vec<DetachedModifierExtension>,
    },
    CarryoverTag {
        tag_type: CarryoverTag,
        name: Vec<String>,
        parameters: Vec<String>,
        next_object: Box<Self>,
    },
    VerbatimRangedTag {
        name: Vec<String>,
        parameters: Vec<String>,
        content: String,
    },
    RangedTag {
        name: Vec<String>,
        parameters: Vec<String>,
        content: Vec<Self>,
    },
    InfirmTag {
        name: Vec<String>,
        parameters: Vec<String>,
    },
    DelimitingModifier(DelimitingModifier),
}

#[derive(Clone, Hash, Debug, PartialEq, Eq, Serialize)]
pub enum DelimitingModifier {
    Weak,
    Strong,
    HorizontalRule,
}

fn detached_modifier_extensions<'a>() -> impl Parser<
    'a,
    &'a [ParagraphSegmentToken],
    Vec<DetachedModifierExtension>,
    extra::Err<Rich<'a, ParagraphSegmentToken>>,
> {
    use ParagraphSegmentToken::*;

    let detached_modifier_extension_tokens = select! {
        c @ Special('@' | '#' | '<' | '>' | '+' | '=' | '_' | '-' | '!') => c,
        Whitespace => Whitespace,
        Text(c) if c == "x" || c == "?" => Text(c),
    };

    let not_pipe = any()
        .filter(|t: &ParagraphSegmentToken| !matches!(t, Special('|')));

    let attached_content = just(Whitespace)
        .ignore_then(not_pipe.repeated().collect::<Vec<_>>())
        .or_not()
        .map(|tokens| {
            if let Some(tokens) = tokens {
                tokens
                    .into_iter()
                    .map_into::<String>()
                    .collect()
            } else {
                String::from("")
            }
        });

    let detached_modifier_extension = detached_modifier_extension_tokens
        .then(attached_content)
        .map(|(spec, metadata)| match spec {
            Special('@') => DetachedModifierExtension::Timestamp(metadata),
            Special('#') => DetachedModifierExtension::Priority(metadata),
            Special('<') => DetachedModifierExtension::DueDate(metadata),
            Special('>') => DetachedModifierExtension::StartDate(metadata),
            Special('+') => {
                DetachedModifierExtension::Todo(TodoStatus::Recurring(if metadata.is_empty() {
                    None
                } else {
                    Some(metadata)
                }))
            }
            Special('=') => DetachedModifierExtension::Todo(TodoStatus::Paused),
            Special('_') => DetachedModifierExtension::Todo(TodoStatus::Canceled),
            Special('-') => DetachedModifierExtension::Todo(TodoStatus::Pending),
            Special('!') => DetachedModifierExtension::Todo(TodoStatus::Urgent),
            Whitespace => DetachedModifierExtension::Todo(TodoStatus::Undone),
            Text(str) if str == "x" => DetachedModifierExtension::Todo(TodoStatus::Done),
            Text(str) if str == "?" => {
                DetachedModifierExtension::Todo(TodoStatus::NeedsClarification)
            }
            _ => unreachable!(),
        });

    detached_modifier_extension
        .separated_by(just(Special('|')))
        .at_least(1)
        .collect::<Vec<_>>()
}

pub fn stage_3<'src>(
) -> impl Parser<'src, &'src [NorgBlock], Vec<NorgASTFlat>, extra::Err<Rich<'src, NorgBlock>>> {
    recursive::<_, _, extra::Err<Rich<'src, NorgBlock>>, _, _>(|stage_3| {
        let paragraph_segment = select! {
            NorgBlock::ParagraphSegment(content) => content,
        };

        let paragraph_segment_end = select! {
            NorgBlock::ParagraphSegmentEnd(content) => content,
        };

        let paragraph = choice((
            paragraph_segment
                .repeated()
                .at_least(1)
                .collect::<Vec<_>>()
                .then(paragraph_segment_end.or_not())
                .map(|(segments, end)| {
                    let mut tokens: Vec<ParagraphSegmentToken> = segments.into_iter().flatten().collect();
                    if let Some(end) = end {
                        tokens.extend(end);
                    }
                    tokens
                }),
            paragraph_segment_end
                .map(|end| end),
        ))
            .map(|mut tokens| {
                if let Some(ParagraphSegmentToken::Whitespace) = tokens.last() {
                    tokens.pop();
                }

                NorgASTFlat::Paragraph(parse_paragraph(tokens))
            });

        let nestable_detached_modifier = select! {
            NorgBlock::NestableDetachedModifier { modifier_type: '-', level, extension_section } => (NestableDetachedModifier::UnorderedList, level, extension_section),
            NorgBlock::NestableDetachedModifier { modifier_type: '~', level, extension_section } => (NestableDetachedModifier::OrderedList, level, extension_section),
            NorgBlock::NestableDetachedModifier { modifier_type: '>', level, extension_section } => (NestableDetachedModifier::Quote, level, extension_section),
        }.then(paragraph).map(|((modifier_type, level, extension_section), paragraph)| NorgASTFlat::NestableDetachedModifier {
                modifier_type,
                level,
                extensions: detached_modifier_extensions().parse(&extension_section[..]).into_result().unwrap_or_default(),
                content: Box::new(paragraph),
            });

        let nonranged_detached_modifier = select! {
            NorgBlock::RangeableDetachedModifier { modifier_type: '$', ranged: false, title, extension_section } => (RangeableDetachedModifier::Definition, title, extension_section),
            NorgBlock::RangeableDetachedModifier { modifier_type: '^', ranged: false, title, extension_section} => (RangeableDetachedModifier::Footnote, title, extension_section),
            NorgBlock::RangeableDetachedModifier { modifier_type: ':', ranged: false, title, extension_section } => (RangeableDetachedModifier::Table, title, extension_section),
        }.then(paragraph).map(|((modifier_type, title, extension_section), paragraph)| NorgASTFlat::RangeableDetachedModifier {
                modifier_type,
                title: parse_paragraph(title),
                extensions: detached_modifier_extensions().parse(&extension_section[..]).into_result().unwrap_or_default(),
                content: vec![paragraph],
            });

        let stage_3_ref = stage_3.clone();

        let ranged_detached_modifier = select! {
            NorgBlock::RangeableDetachedModifier { modifier_type: '$', ranged: true, title, extension_section } => ('$', RangeableDetachedModifier::Definition, title, extension_section),
            NorgBlock::RangeableDetachedModifier { modifier_type: '^', ranged: true, title, extension_section } => ('^', RangeableDetachedModifier::Footnote, title, extension_section),
            NorgBlock::RangeableDetachedModifier { modifier_type: ':', ranged: true, title, extension_section } => (':', RangeableDetachedModifier::Table, title, extension_section),
        }
            .then(stage_3_ref.repeated().collect::<Vec<_>>())
            .then(select! { NorgBlock::RangeableDetachedModifierClose(c) => c })
            .try_map(|(((opening_ch, modifier_type, title, extension_section), content), closing_ch), span|
                if opening_ch == closing_ch {
                    Ok(NorgASTFlat::RangeableDetachedModifier {
                        modifier_type,
                        title: parse_paragraph(title),
                        extensions: detached_modifier_extensions().parse(&extension_section[..]).into_result().unwrap_or_default(),
                        content,
                    })
                } else {
                    Err(Rich::custom(span, format!("Expected '{0}{0}' to close modifier, found '{1}{1}' instead.", opening_ch, closing_ch)))
                });

        let heading = select! {
            NorgBlock::Heading { level, title, extension_section } => (level, title, extension_section),
        }
        .try_map(move |(level, title, extension_section), _span| Ok(NorgASTFlat::Heading {
            level,
            title: parse_paragraph(title),
            extensions: detached_modifier_extensions().parse(&extension_section[..]).into_result().unwrap_or_default(),
        }));

        let stringify_tokens_and_split = move |tokens: ParagraphTokenList| -> Vec<String> {
            tokens.into_iter().map_into::<String>().collect::<String>().split('.').map_into().collect()
        };

        let stage_3_ref2 = stage_3.clone();

        let carryover_tag = select! {
            NorgBlock::CarryoverTag { tag_type: '+', name, parameters } => (CarryoverTag::Attribute, name, parameters),
            NorgBlock::CarryoverTag { tag_type: '#', name, parameters } => (CarryoverTag::Macro, name, parameters),
        }.then(stage_3_ref2).map(move |((tag_type, name, parameters), next_object)| {
                NorgASTFlat::CarryoverTag {
                    tag_type,
                    name: stringify_tokens_and_split(name),
                    parameters: parameters.unwrap_or_default().into_iter().map(|parameter| parameter.into_iter().map_into::<String>().collect()).collect(),
                    next_object: Box::new(next_object),
                }
            });

        let verbatim_ranged_tag = select! {
            NorgBlock::VerbatimRangedTag { name, parameters, content } => {
                NorgASTFlat::VerbatimRangedTag {
                    name: stringify_tokens_and_split(name),
                    parameters: parameters.unwrap_or_default().into_iter().map(|parameter| parameter.into_iter().map_into::<String>().collect()).collect(),
                    content: dedent(content.into_iter().map_into::<String>().collect::<String>().as_str()),
                }
            },
        };

        let stage_3_ref3 = stage_3.clone();

        let ranged_tag = select! {
            NorgBlock::RangedTag { tag_type: '=', name, parameters } => (RangedTag::Macro, stringify_tokens_and_split(name), parameters.unwrap_or_default().into_iter().map(|parameter| parameter.into_iter().map_into::<String>().collect()).collect()),
            NorgBlock::RangedTag { tag_type: '|', name, parameters } => (RangedTag::Standard, stringify_tokens_and_split(name), parameters.unwrap_or_default().into_iter().map(|parameter| parameter.into_iter().map_into::<String>().collect()).collect())
        }.then(stage_3_ref3.repeated().collect::<Vec<_>>()).then(select! {
            NorgBlock::RangedTagEnd('=') => RangedTag::Macro,
            NorgBlock::RangedTagEnd('|') => RangedTag::Standard,
        }).try_map(|(((tag_type, name, parameters), content), closing_tag_type), span| if tag_type == closing_tag_type {
            Ok(NorgASTFlat::RangedTag { name, parameters, content })
        } else {
            Err(Rich::custom(span, "Invalid closing modifier for ranged tag."))
        });

        let infirm_tag = select! {
            NorgBlock::InfirmTag { name, parameters, } => NorgASTFlat::InfirmTag { name: stringify_tokens_and_split(name), parameters: parameters.unwrap_or_default().into_iter().map(|parameter| parameter.into_iter().map_into::<String>().collect()).collect() },
        };

        let delimiting_mod = select! {
            NorgBlock::DelimitingModifier('-') => NorgASTFlat::DelimitingModifier(DelimitingModifier::Weak),
            NorgBlock::DelimitingModifier('=') => NorgASTFlat::DelimitingModifier(DelimitingModifier::Strong),
            NorgBlock::DelimitingModifier('_') => NorgASTFlat::DelimitingModifier(DelimitingModifier::HorizontalRule),
        };

        choice((
            carryover_tag,
            verbatim_ranged_tag,
            ranged_tag,
            infirm_tag,
            delimiting_mod,
            heading,
            nestable_detached_modifier,
            nonranged_detached_modifier,
            ranged_detached_modifier,
            paragraph,
        ))
    }).repeated().at_least(1).collect::<Vec<_>>()
}