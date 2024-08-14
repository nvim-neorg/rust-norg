use chumsky::prelude::*;
use itertools::Itertools;
use serde::Serialize;

use crate::stage_2::{NorgBlock, ParagraphSegmentToken, ParagraphTokenList};

#[derive(Debug, PartialEq, Serialize)]
pub enum NestableDetachedModifier {
    Quote,
    UnorderedList,
    OrderedList,
}

#[derive(Debug, PartialEq, Serialize)]
pub enum RangeableDetachedModifier {
    Definition,
    Footnote,
    Table,
}

#[derive(Debug, PartialEq, Serialize)]
pub enum TodoStatus {
    /// ` `
    Undone,
    /// `x`
    Done,
    /// `?`
    NeedsClarification,
    /// `=`
    Paused,
    /// `!`
    Urgent,
    /// `+` or `+ 4th may`
    Recurring(Option<String>),
    /// `-`
    Pending,
    /// `_`
    Canceled,
}

#[derive(Debug, PartialEq, Serialize)]
pub enum DetachedModifierExtension {
    /// todo item status:
    /// `- ( ) undone`
    /// `- (x) done`
    /// `- (?) needs clarification`
    /// `- (=) paused/on hold`
    /// `- (!) urgent`
    /// `- (+) recurring`
    /// `- (+ 15th May) recurring with a time stamp`
    /// `- (-) in progress/pending`
    /// `- (_) put down/canceled`
    Todo(TodoStatus),

    /// Priority, `#` and then any text
    /// `- (# A) Priority A`
    Priority(String),

    /// Time stamp extension:
    /// `- (@ <some time>) list item text`
    Timestamp(String),

    /// Time stamp for the due date/deadline for this item
    /// `- (< 1 Jan 2025) Do something`
    DueDate(String),

    /// Time stamp for the start time of the item:
    /// `- (> 2 Jan 2025)` Start something
    StartDate(String),
}

#[derive(Debug, PartialEq, Serialize)]
pub enum CarryoverTag {
    Attribute, // `+`
    Macro,     // `#`
}

#[derive(PartialEq, Serialize)]
pub enum RangedTag {
    Macro,
    Standard,
}

fn paragraph_parser_opener_candidates_and_links() -> impl Parser<
    ParagraphSegmentToken,
    Vec<ParagraphSegment>,
    Error = chumsky::error::Simple<ParagraphSegmentToken>,
> {
    let token = any().map(ParagraphSegment::Token);
    let modifier = select! {
        ParagraphSegmentToken::Special(c @ ('*' | '/' | '_' | '-')) => c,
    };

    let whitespace_or_special = select! {
        w @ ParagraphSegmentToken::Whitespace => w,
        s @ ParagraphSegmentToken::Special(_) => s,
    };

    let opening_modifier_candidate = whitespace_or_special
        .then(modifier.repeated().at_least(1))
        .then(just(ParagraphSegmentToken::Whitespace).not())
        .map(|((left, modifiers), right)| {
            ParagraphSegment::AttachedModifierOpener((Some(left), modifiers, right))
        });

    let left_empty_opening_modifier = modifier
        .repeated()
        .at_least(1)
        .then(just(ParagraphSegmentToken::Whitespace).not())
        .map(|(modifiers, right)| {
            ParagraphSegment::AttachedModifierOpener((None, modifiers, right))
        });

    let anchor = just(ParagraphSegmentToken::Special('['))
        .ignore_then(
            just(ParagraphSegmentToken::Special(']'))
                .not()
                .repeated()
                .at_least(1),
        )
        .then_ignore(just(ParagraphSegmentToken::Special(']')));

    let link = just(ParagraphSegmentToken::Special('{'))
        .ignore_then(
            just(ParagraphSegmentToken::Special(':'))
                .ignore_then(
                    just(ParagraphSegmentToken::Special(':'))
                        .not()
                        .repeated()
                        .at_least(1),
                )
                .then_ignore(just(ParagraphSegmentToken::Special(':')))
                .or_not(),
        )
        .then(
            choice((
                just(ParagraphSegmentToken::Special('*'))
                    .repeated()
                    .at_least(1)
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
            just(ParagraphSegmentToken::Special('}'))
                .not()
                .repeated()
                .at_least(1),
        )
        .then_ignore(just(ParagraphSegmentToken::Special('}')))
        .then(anchor.clone().or_not())
        .map(
            |(((filepath, modifiers), content), description)| ParagraphSegment::Link {
                filepath: filepath
                    .map(|content| content.into_iter().map_into::<String>().collect()),
                description: description.map(|content| parse_paragraph(content).unwrap()),
                targets: vec![if let Some(modifiers) = modifiers {
                    match modifiers.as_str() {
                        "$" => LinkTarget::Definition(parse_paragraph(content).unwrap()),
                        "^" => LinkTarget::Footnote(parse_paragraph(content).unwrap()),
                        "?" => LinkTarget::Wiki(parse_paragraph(content).unwrap()),
                        "=" => LinkTarget::Extendable(parse_paragraph(content).unwrap()),
                        "/" => LinkTarget::Path(content.into_iter().map_into::<String>().collect()),
                        "@" => LinkTarget::Timestamp(
                            content.into_iter().map_into::<String>().collect(),
                        ),

                        // Only other possibility is a heading.
                        str => LinkTarget::Heading {
                            level: str.len() as u16,
                            title: parse_paragraph(content).unwrap(),
                        },
                    }
                } else {
                    LinkTarget::Url(content.into_iter().map_into::<String>().collect())
                }],
            },
        );

    let inline_linkable = just(ParagraphSegmentToken::Special('<'))
        .ignore_then(
            just(ParagraphSegmentToken::Special('>'))
                .not()
                .repeated()
                .at_least(1),
        )
        .then_ignore(just(ParagraphSegmentToken::Special('>')))
        .map(|content| ParagraphSegment::InlineLinkTarget(parse_paragraph(content).unwrap()));

    left_empty_opening_modifier.or_not().chain(
        choice((
            link.clone(),
            anchor
                .clone()
                .then(link)
                .map(|(content, link)| ParagraphSegment::AnchorDefinition {
                    content: parse_paragraph(content).unwrap(),
                    target: Box::new(link),
                }),
            anchor
                .clone()
                .then(anchor.clone().or_not())
                .map(|(content, description)| ParagraphSegment::Anchor {
                    content: parse_paragraph(content).unwrap(),
                    description: description.map(|content| parse_paragraph(content).unwrap()),
                }),
            inline_linkable,
            opening_modifier_candidate,
            token,
        ))
        .repeated()
        .at_least(1),
    )
}

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

fn paragraph_parser_closer_candidates(
) -> impl Parser<ParagraphSegment, Vec<ParagraphSegment>, Error = chumsky::error::Simple<ParagraphSegment>>
{
    use ParagraphSegment::*;

    let token = any();
    let modifier = select! {
        Token(ParagraphSegmentToken::Special(c @ ('*' | '/' | '_' | '-'))) => c,
    };

    let whitespace_or_special = select! {
        w @ Token(ParagraphSegmentToken::Whitespace) => w,
        s @ Token(ParagraphSegmentToken::Special(_)) => s,
    };

    let closing_modifier_candidate = just(Token(ParagraphSegmentToken::Whitespace))
        .not()
        .then(modifier.repeated().at_least(1))
        .then(whitespace_or_special)
        .map(|((left, modifiers), right)| {
            ParagraphSegment::AttachedModifierCloserCandidate((
                Box::new(left),
                modifiers,
                Some(Box::new(right)),
            ))
        });

    // TODO(vhyrro): This is not optimal, as it causes a second parse of a potentially long string
    // of nodes. Ideally, the `end()` check should be done directly in a single parse.
    let closing_modifier_candidate_with_eof = just(Token(ParagraphSegmentToken::Whitespace))
        .not()
        .then(modifier.repeated().at_least(1))
        .then_ignore(end())
        .map(|(left, modifiers)| {
            ParagraphSegment::AttachedModifierCloserCandidate((Box::new(left), modifiers, None))
        });

    choice((
        closing_modifier_candidate,
        closing_modifier_candidate_with_eof,
        token,
    ))
    .repeated()
    .at_least(1)
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

fn paragraph_rollup_candidates(
) -> impl Parser<ParagraphSegment, Vec<ParagraphSegment>, Error = chumsky::error::Simple<ParagraphSegment>>
{
    let candidate = select! { ParagraphSegment::AttachedModifierCloser(c) => c, };

    let attached_modifier = recursive(|attached_modifier| {
        select! {
            ParagraphSegment::AttachedModifierCandidate { modifier_type, .. } => modifier_type,
        }
        .then(attached_modifier.or(candidate.not()).repeated().at_least(1))
        .then(candidate)
        .try_map(|((modifier_type, content), closer), span| {
            if modifier_type == closer {
                Ok(ParagraphSegment::AttachedModifier {
                    modifier_type,
                    content,
                })
            } else {
                Err(Simple::custom(
                    span,
                    "differing opening and closing modifiers found",
                ))
            }
        })
    });

    choice((attached_modifier, any())).repeated().at_least(1)
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
}

fn parse_paragraph(
    input: Vec<ParagraphSegmentToken>,
) -> Result<Vec<ParagraphSegment>, Vec<chumsky::error::Simple<ParagraphSegmentToken>>> {
    Ok(eliminate_invalid_candidates(unravel_candidates(
        paragraph_rollup_candidates()
            .parse(unravel_candidates(
                paragraph_parser_closer_candidates()
                    .parse(unravel_candidates(dedup_opener_candidates(
                        paragraph_parser_opener_candidates_and_links().parse(input)?,
                    )))
                    .unwrap(),
            ))
            .unwrap(),
    )))
}

#[derive(Debug, PartialEq, Serialize)]
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
        title: ParagraphTokenList,
        extensions: Vec<DetachedModifierExtension>,
        content: Vec<Self>,
    },
    Heading {
        level: u16,
        title: ParagraphTokenList,
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
}

fn detached_modifier_extensions() -> impl Parser<
    ParagraphSegmentToken,
    Vec<DetachedModifierExtension>,
    Error = chumsky::error::Simple<ParagraphSegmentToken>,
> {
    use ParagraphSegmentToken::*;

    let detached_modifier_extension_tokens = select! {
        c @ Special('@' | '#' | '<' | '>' | '+' | '=' | '_' | '-' | '!') => c,
        Whitespace => Whitespace,
        Text(c) if c == "x" || c == "?" => Text(c),
    };

    let detached_modifier_extension = detached_modifier_extension_tokens
        .then(
            just(Whitespace)
                .ignore_then(select!(Special('|') => Special('|')).repeated())
                .or_not()
                .map(|tokens| {
                    if let Some(tokens) = tokens {
                        tokens
                            .iter()
                            .map(|spec| match spec {
                                Special(char) => char.to_string(),
                                Text(str) => str.to_owned(),
                                Escape(char) => format!(r"\{}", char),
                                Whitespace => ' '.to_string(),
                            })
                            .collect()
                    } else {
                        String::from("")
                    }
                }),
        )
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
}

pub fn stage_3(
) -> impl Parser<NorgBlock, Vec<NorgASTFlat>, Error = chumsky::error::Simple<NorgBlock>> {
    recursive(|stage_3| {
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
                .flatten()
                .chain(paragraph_segment_end.or_not()),
            paragraph_segment_end,
        ))
            .map(|mut tokens| {
                // Trim trailing whitespace (both user-induced but also induced by us when
                // converting single newlines to whitespace).
                if let Some(ParagraphSegmentToken::Whitespace) = tokens.last() {
                    tokens.pop();
                }

                NorgASTFlat::Paragraph(parse_paragraph(tokens).unwrap())
            });

        let nestable_detached_modifier = select! {
            NorgBlock::NestableDetachedModifier { modifier_type: '-', level, extension_section } => (NestableDetachedModifier::UnorderedList, level, extension_section),
            NorgBlock::NestableDetachedModifier { modifier_type: '~', level, extension_section } => (NestableDetachedModifier::OrderedList, level, extension_section),
            NorgBlock::NestableDetachedModifier { modifier_type: '>', level, extension_section } => (NestableDetachedModifier::Quote, level, extension_section),
        }.then(paragraph).map(|((modifier_type, level, extension_section), paragraph)| NorgASTFlat::NestableDetachedModifier {
                modifier_type,
                level,
                extensions: detached_modifier_extensions().parse(extension_section).unwrap_or_default(),
                content: Box::new(paragraph),
            });

        let nonranged_detached_modifier = select! {
            NorgBlock::RangeableDetachedModifier { modifier_type: '$', ranged: false, title, extension_section } => (RangeableDetachedModifier::Definition, title, extension_section),
            NorgBlock::RangeableDetachedModifier { modifier_type: '^', ranged: false, title, extension_section} => (RangeableDetachedModifier::Footnote, title, extension_section),
            NorgBlock::RangeableDetachedModifier { modifier_type: ':', ranged: false, title, extension_section } => (RangeableDetachedModifier::Table, title, extension_section),
        }.then(paragraph).map(|((modifier_type, title, extension_section), paragraph)| NorgASTFlat::RangeableDetachedModifier {
                modifier_type,
                title,
                extensions: detached_modifier_extensions().parse(extension_section).unwrap_or_default(),
                content: vec![paragraph],
            });

        let ranged_detached_modifier = select! {
            NorgBlock::RangeableDetachedModifier { modifier_type: '$', ranged: true, title, extension_section } => ('$', RangeableDetachedModifier::Definition, title, extension_section),
            NorgBlock::RangeableDetachedModifier { modifier_type: '^', ranged: true, title, extension_section } => ('^', RangeableDetachedModifier::Footnote, title, extension_section),
            NorgBlock::RangeableDetachedModifier { modifier_type: ':', ranged: true, title, extension_section } => (':', RangeableDetachedModifier::Table, title, extension_section),
        }
            .then(stage_3.clone().repeated())
            .then(select! { NorgBlock::RangeableDetachedModifierClose(c) => c })
            .try_map(|(((opening_ch, modifier_type, title, extension_section), content), closing_ch), span|
                if opening_ch == closing_ch {
                    Ok(NorgASTFlat::RangeableDetachedModifier {
                        modifier_type,
                        title,
                        extensions: detached_modifier_extensions().parse(extension_section).unwrap_or_default(),
                        content,
                    })
                } else {
                    Err(Simple::custom(span, format!("Expected '{0}{0}' to close modifier, found '{1}{1}' instead.", opening_ch, closing_ch)))
                });

        let heading = select! {
            NorgBlock::Heading { level, title, extension_section } => (level, title, extension_section),
        }
        .try_map(move |(level, title, extension_section), _span| Ok(NorgASTFlat::Heading {
            level,
            title,
            extensions: detached_modifier_extensions().parse(extension_section).unwrap_or_default(),
        }));

        let stringify_tokens_and_split = move |tokens: ParagraphTokenList| -> Vec<String> {
            tokens.into_iter().map_into::<String>().collect::<String>().split('.').map_into().collect()
        };

        let carryover_tag = select! {
            NorgBlock::CarryoverTag { tag_type: '+', name, parameters } => (CarryoverTag::Attribute, name, parameters),
            NorgBlock::CarryoverTag { tag_type: '#', name, parameters } => (CarryoverTag::Macro, name, parameters),
        }.then(stage_3.clone()).map(move |((tag_type, name, parameters), next_object)| {
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
                    content: content.into_iter().map_into::<String>().collect(),
                }
            },
        };

        let ranged_tag = select! {
            NorgBlock::RangedTag { tag_type: '=', name, parameters } => (RangedTag::Macro, stringify_tokens_and_split(name), parameters.unwrap_or_default().into_iter().map(|parameter| parameter.into_iter().map_into::<String>().collect()).collect()),
            NorgBlock::RangedTag { tag_type: '|', name, parameters } => (RangedTag::Standard, stringify_tokens_and_split(name), parameters.unwrap_or_default().into_iter().map(|parameter| parameter.into_iter().map_into::<String>().collect()).collect())
        }.then(stage_3.repeated()).then(select! {
            NorgBlock::RangedTagEnd('=') => RangedTag::Macro,
            NorgBlock::RangedTagEnd('|') => RangedTag::Standard,
        }).try_map(|(((tag_type, name, parameters), content), closing_tag_type), span| if tag_type == closing_tag_type {
            Ok(NorgASTFlat::RangedTag { name, parameters, content })
        } else {
            Err(Simple::custom(span, "Invalid closing modifier for ranged tag.")) // TODO: Improve errors
        });

        let infirm_tag = select! {
            NorgBlock::InfirmTag { name, parameters, } => NorgASTFlat::InfirmTag { name: stringify_tokens_and_split(name), parameters: parameters.unwrap_or_default().into_iter().map(|parameter| parameter.into_iter().map_into::<String>().collect()).collect() },
        };

        choice((
            carryover_tag,
            verbatim_ranged_tag,
            ranged_tag,
            infirm_tag,
            heading,
            nestable_detached_modifier,
            nonranged_detached_modifier,
            ranged_detached_modifier,
            paragraph,
        ))
    }).repeated().at_least(1)
}
