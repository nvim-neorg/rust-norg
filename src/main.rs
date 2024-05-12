use chumsky::prelude::*;
use chumsky::text::{keyword, Character};
use chumsky::Parser;
use clap::Parser as ClapParser;
use eyre::Result;
use itertools::Itertools;
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum NorgBlock {
    ParagraphSegment(Vec<NorgToken>),
    ParagraphSegmentEnd(Vec<NorgToken>),
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
    .map(
        |(modifier_type, level)| NorgBlock::NestableDetachedModifier {
            modifier_type,
            level,
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
                |((name, parameters), content)| NorgBlock::VerbatimRangedTag {
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

#[derive(Debug, Clone, PartialEq)]
enum NestableDetachedModifier {
    Quote,
    UnorderedList,
    OrderedList,
}

#[derive(Debug, Clone, PartialEq)]
enum RangeableDetachedModifier {
    Definition,
    Footnote,
    Table,
}

#[derive(Debug, Clone, PartialEq)]
enum CarryoverTag {
    Attribute, // `+`
    Macro,     // `#`
}

#[derive(Debug, Clone, PartialEq)]
enum RangedTag {
    Macro,
    Standard,
}

#[derive(Debug, Clone, PartialEq)]
enum NorgASTFlat {
    Paragraph(Vec<NorgToken>),
    NestableDetachedModifier {
        modifier_type: NestableDetachedModifier,
        level: u16,
        content: Box<Self>,
    },
    RangeableDetachedModifier {
        modifier_type: RangeableDetachedModifier,
        title: Vec<NorgToken>,
        content: Vec<Self>,
    },
    Heading {
        level: u16,
        title: Vec<NorgToken>,
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

fn stage_3() -> impl Parser<NorgBlock, Vec<NorgASTFlat>, Error = chumsky::error::Simple<NorgBlock>>
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
            NorgBlock::NestableDetachedModifier { modifier_type: '-', level } => (NestableDetachedModifier::UnorderedList, level),
            NorgBlock::NestableDetachedModifier { modifier_type: '~', level } => (NestableDetachedModifier::OrderedList, level),
            NorgBlock::NestableDetachedModifier { modifier_type: '>', level } => (NestableDetachedModifier::Quote, level),
        }.then(paragraph).map(|((modifier_type, level), paragraph)| NorgASTFlat::NestableDetachedModifier { modifier_type, level, content: Box::new(paragraph), });

        let nonranged_detached_modifier = select! {
            NorgBlock::RangeableDetachedModifier { modifier_type: '$', ranged: false, title } => (RangeableDetachedModifier::Definition, title),
            NorgBlock::RangeableDetachedModifier { modifier_type: '^', ranged: false, title } => (RangeableDetachedModifier::Footnote, title),
            NorgBlock::RangeableDetachedModifier { modifier_type: ':', ranged: false, title } => (RangeableDetachedModifier::Table, title),
        }.then(paragraph).map(|((modifier_type, title), paragraph)| NorgASTFlat::RangeableDetachedModifier { modifier_type, title, content: Vec::from([paragraph]), });

        let ranged_detached_modifier = select! {
            NorgBlock::RangeableDetachedModifier { modifier_type: '$', ranged: true, title } => ('$', RangeableDetachedModifier::Definition, title),
            NorgBlock::RangeableDetachedModifier { modifier_type: '^', ranged: true, title } => ('^', RangeableDetachedModifier::Footnote, title),
            NorgBlock::RangeableDetachedModifier { modifier_type: ':', ranged: true, title } => (':', RangeableDetachedModifier::Table, title),
        }.then(stage_3.clone().repeated()).then(select! { NorgBlock::RangeableDetachedModifierClose(c) => c }).try_map(|(((opening_ch, modifier_type, title), content), closing_ch), span| if opening_ch == closing_ch {
            Ok(NorgASTFlat::RangeableDetachedModifier { modifier_type, title, content })
        } else {
            Err(Simple::custom(span, format!("Expected '{0}{0}' to close modifier, found '{1}{1}' instead.", opening_ch, closing_ch)))
        });

        let heading = select! {
            NorgBlock::Heading { level, title } => (level, title),
        }
        .then(stage_3.clone().repeated())
        .map(|((level, title), content)| NorgASTFlat::Heading {
            level,
            title,
            content,
        });

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
            NorgASTFlat::CarryoverTag { tag_type, name: stringify_tokens_and_split(name), parameters: parameters.unwrap_or_else(Vec::new).into_iter().map(stringify_tokens).collect(), next_object: Box::new(next_object) }
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

fn main() -> Result<()> {
    let parser = Norg::parse();

    let content = String::from_utf8(std::fs::read(parser.file)?)?;

    println!(
        "{:#?}",
        block_level()
            .parse_recovery(lexer().parse_recovery(content.clone()).0.expect("Failed"))
            .0
            .expect("failed stage 2")
    );

    println!("-----------------------");

    println!(
        "{:#?}",
        stage_3().parse(
            block_level()
                .parse(lexer().parse_recovery(content).0.expect("Failed"))
                .unwrap()
        ).unwrap()
    );

    Ok(())
}
