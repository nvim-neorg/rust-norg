use chumsky::prelude::*;
use itertools::Itertools;
use serde::Serialize;

use crate::{stage_1::NorgToken, stage_2::NorgBlock};

#[derive(Debug, Clone, PartialEq, Serialize)]
pub(crate) enum NestableDetachedModifier {
    Quote,
    UnorderedList,
    OrderedList,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub(crate) enum RangeableDetachedModifier {
    Definition,
    Footnote,
    Table,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub(crate) enum TodoStatus {
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

#[derive(Debug, Clone, PartialEq, Serialize)]
pub(crate) enum DetachedModifierExtension {
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

#[derive(Debug, Clone, PartialEq, Serialize)]
pub(crate) enum CarryoverTag {
    Attribute, // `+`
    Macro,     // `#`
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub(crate) enum RangedTag {
    Macro,
    Standard,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum NorgASTFlat {
    Paragraph(Vec<NorgToken>),
    NestableDetachedModifier {
        modifier_type: NestableDetachedModifier,
        level: u16,
        extensions: Vec<DetachedModifierExtension>,
        content: Box<Self>,
    },
    RangeableDetachedModifier {
        modifier_type: RangeableDetachedModifier,
        title: Vec<NorgToken>,
        extensions: Vec<DetachedModifierExtension>,
        content: Vec<Self>,
    },
    Heading {
        level: u16,
        title: Vec<NorgToken>,
        extensions: Vec<DetachedModifierExtension>,
        content: Vec<Self>,
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
        content: Vec<NorgToken>,
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

fn detached_modifier_extensions(
) -> impl Parser<NorgToken, Vec<DetachedModifierExtension>, Error = chumsky::error::Simple<NorgToken>>
{
    use NorgToken::*;

    let detached_modifier_extension_tokens = select! {
        c @ Special('@' | '#' | '<' | '>' | '+' | '=' | '_' | '-' | '!') => c,
        Whitespace => Whitespace,
        Text(str) if str == "x" || str == "?" => Text(str),
    };

    let detached_modifier_extension = detached_modifier_extension_tokens
        .then(
            just(Whitespace)
                .ignore_then(none_of([Special('|'), Eof, SingleNewline, Newlines]).repeated())
                .or_not()
                .map(|tokens| {
                    if let Some(tokens) = tokens {
                        tokens
                            .iter()
                            .filter_map(|spec| match spec {
                                Special(char) => Some(char.to_string()),
                                Text(txt) => Some(txt.to_string()),
                                Escape(char) => Some(char.to_string()),
                                Whitespace => Some(" ".to_string()),
                                _ => None,
                            })
                            .join("")
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
            Text(x) if x == "x" => DetachedModifierExtension::Todo(TodoStatus::Done),
            Text(x) if x == "?" => DetachedModifierExtension::Todo(TodoStatus::NeedsClarification),
            _ => unreachable!(),
        });

    detached_modifier_extension
        .separated_by(just(Special('|')))
        .at_least(1)
}

pub fn stage_3() -> impl Parser<NorgBlock, Vec<NorgASTFlat>, Error = chumsky::error::Simple<NorgBlock>>
{
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
            .map(NorgASTFlat::Paragraph);

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
                content: Vec::from([paragraph]),
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

        let stage_3_clone = stage_3.clone();

        // TODO(vhyrro): Proper error handling here, properly make headings non-self-consuming
        let heading = recursive(|heading| select! {
            NorgBlock::Heading { level, title, extension_section } => (level, title, extension_section),
        }
        .then(heading.not().repeated())
        .try_map(move |((level, title, extension_section), content), _span| Ok(NorgASTFlat::Heading {
            level,
            title,
            extensions: detached_modifier_extensions().parse(extension_section).unwrap_or_default(),
            content: stage_3_clone.clone().repeated().parse(content).unwrap_or_default(),
        })));

        let stringify_tokens = |tokens: Vec<NorgToken>| -> String {
            tokens.into_iter().map(|token| match token {
                NorgToken::Text(txt) => txt,
                NorgToken::Special(c) | NorgToken::Escape(c) => c.to_string(),
                _ => unreachable!(),
            }).collect::<String>()
        };

        let stringify_tokens_and_split = move |tokens: Vec<NorgToken>| -> Vec<String> {
            stringify_tokens(tokens).split('.').map_into().collect()
        };

        let carryover_tag = select! {
            NorgBlock::CarryoverTag { tag_type: '+', name, parameters } => (CarryoverTag::Attribute, name, parameters),
            NorgBlock::CarryoverTag { tag_type: '#', name, parameters } => (CarryoverTag::Macro, name, parameters),
        }.then(stage_3.clone()).map(move |((tag_type, name, parameters), next_object)| {
                NorgASTFlat::CarryoverTag {
                    tag_type,
                    name: stringify_tokens_and_split(name),
                    parameters: parameters.unwrap_or_else(Vec::new).into_iter().map(stringify_tokens).collect(),
                    next_object: Box::new(next_object),
                }
            });

        let verbatim_ranged_tag = select! {
            NorgBlock::VerbatimRangedTag { name, parameters, content } => {
                NorgASTFlat::VerbatimRangedTag {
                    name: stringify_tokens_and_split(name),
                    parameters: parameters.unwrap_or_else(Vec::new).into_iter().map(stringify_tokens).collect(),
                    content,
                }
            },
        };

        let ranged_tag = select! {
            NorgBlock::RangedTag { tag_type: '=', name, parameters } => (RangedTag::Macro, stringify_tokens_and_split(name), parameters.unwrap_or_else(Vec::new).into_iter().map(stringify_tokens).collect()),
            NorgBlock::RangedTag { tag_type: '|', name, parameters } => (RangedTag::Standard, stringify_tokens_and_split(name), parameters.unwrap_or_else(Vec::new).into_iter().map(stringify_tokens).collect())
        }.then(stage_3.repeated()).then(select! {
            NorgBlock::RangedTagEnd('=') => RangedTag::Macro,
            NorgBlock::RangedTagEnd('|') => RangedTag::Standard,
        }).try_map(|(((tag_type, name, parameters), content), closing_tag_type), span| if tag_type == closing_tag_type {
                Ok(NorgASTFlat::RangedTag { name, parameters, content })
            } else {
                    Err(Simple::custom(span, "Invalid closing modifier for ranged tag.")) // TODO: Improve errors
                });

        let infirm_tag = select! {
            NorgBlock::InfirmTag { name, parameters, } => NorgASTFlat::InfirmTag { name: stringify_tokens_and_split(name), parameters: parameters.unwrap_or_else(Vec::new).into_iter().map(stringify_tokens).collect() },
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
