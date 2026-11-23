use serde::Serialize;

use crate::{
    stage_3::{DelimitingModifier, NorgASTFlat, ParagraphSegment},
    CarryoverTag, DetachedModifierExtension, NestableDetachedModifier, RangeableDetachedModifier,
};

#[derive(Debug, PartialEq, Eq, Clone, Hash, Serialize)]
pub enum NorgAST {
    Paragraph(Vec<ParagraphSegment>),
    NestableDetachedModifier {
        modifier_type: NestableDetachedModifier,
        level: u16,
        extensions: Vec<DetachedModifierExtension>,
        text: Box<NorgASTFlat>,
        content: Vec<Self>,
    },
    List {
        modifier_type: NestableDetachedModifier,
        // These will only ever be NestableDetachedModifiers, we should really pull them out into
        // their own struct
        items: Vec<NorgAST>,
    },
    RangeableDetachedModifier {
        modifier_type: RangeableDetachedModifier,
        title: Vec<ParagraphSegment>,
        extensions: Vec<DetachedModifierExtension>,
        content: Vec<NorgASTFlat>,
    },
    Heading {
        level: u16,
        title: Vec<ParagraphSegment>,
        extensions: Vec<DetachedModifierExtension>,
        content: Vec<Self>,
    },
    CarryoverTag {
        tag_type: CarryoverTag,
        name: Vec<String>,
        parameters: Vec<String>,
        next_object: Box<NorgAST>,
    },
    VerbatimRangedTag {
        name: Vec<String>,
        parameters: Vec<String>,
        content: String,
    },
    RangedTag {
        name: Vec<String>,
        parameters: Vec<String>,
        content: Vec<NorgASTFlat>,
    },
    InfirmTag {
        name: Vec<String>,
        parameters: Vec<String>,
    },
    DelimitingModifier(DelimitingModifier),
}

fn convert(flat: NorgASTFlat) -> NorgAST {
    match flat {
        NorgASTFlat::Paragraph(tokens) => NorgAST::Paragraph(tokens),
        NorgASTFlat::RangeableDetachedModifier {
            modifier_type,
            title,
            extensions,
            content,
        } => NorgAST::RangeableDetachedModifier {
            modifier_type,
            title,
            extensions,
            content,
        },
        NorgASTFlat::VerbatimRangedTag {
            name,
            parameters,
            content,
        } => NorgAST::VerbatimRangedTag {
            name,
            parameters,
            content,
        },
        NorgASTFlat::RangedTag {
            name,
            parameters,
            content,
        } => NorgAST::RangedTag {
            name,
            parameters,
            content,
        },
        NorgASTFlat::InfirmTag { name, parameters } => NorgAST::InfirmTag { name, parameters },
        NorgASTFlat::DelimitingModifier(t) => NorgAST::DelimitingModifier(t),
        NorgASTFlat::NestableDetachedModifier {
            modifier_type,
            level,
            extensions,
            content,
        } => NorgAST::NestableDetachedModifier {
            modifier_type,
            level,
            extensions,
            text: content,
            content: vec![],
        },
        NorgASTFlat::Heading {
            level,
            title,
            extensions,
        } => NorgAST::Heading {
            level,
            title,
            extensions,
            content: vec![],
        },
        NorgASTFlat::CarryoverTag {
            tag_type,
            name,
            parameters,
            next_object,
        } => NorgAST::CarryoverTag {
            tag_type,
            name,
            parameters,
            next_object: Box::new(convert(*next_object.clone())),
        },
    }
}

fn consume_heading_content(start_level: &u16, flat: &[NorgASTFlat], i: usize) -> (Vec<NorgAST>, usize) {
    let mut heading_level = *start_level as i16;
    let stop = flat[i + 1..]
        .iter()
        .enumerate()
        .find_map(|(offset, item)| {
            let j = i + 1 + offset;
            match item {
                NorgASTFlat::Heading { level, .. } => {
                    if level <= start_level {
                        Some((j, j - 1))
                    } else {
                        heading_level = *level as i16;
                        None
                    }
                }
                NorgASTFlat::DelimitingModifier(DelimitingModifier::Weak) => {
                    heading_level -= 1;
                    (heading_level < *start_level as i16).then_some((j, j))
                }
                NorgASTFlat::DelimitingModifier(DelimitingModifier::Strong) => Some((j, j)),
                NorgASTFlat::CarryoverTag { next_object, .. } => match &**next_object {
                    NorgASTFlat::Heading { level, .. } if level <= start_level => Some((j, j - 1)),
                    NorgASTFlat::Heading { level, .. } => {
                        heading_level = *level as i16;
                        None
                    }
                    _ => None,
                },
                _ => None,
            }
        });

    match stop {
        Some((j, result_i)) => (stage_4_from(&flat[(i + 1)..j]), result_i),
        None => (stage_4_from(&flat[i + 1..]), flat.len()),
    }
}

/// Loop over the given flat tree from the given index `i` until a non-NestableDetachedModifier is
/// found, OR until a NestableDetachedModifier with level <= the given start_level.
///
/// In English: finds all the stuff that should be in the `content` field of the
/// NorgAST::NestableDetachedModifier, and returns it
///
/// **Mutates** i to be the index in `flat` that we stopped consuming values at.
fn consume_nestable_detached_mod_content(
    start_level: &u16,
    flat: &[NorgASTFlat],
    i: usize,
    list_type: NestableDetachedModifier,
) -> (Vec<NorgAST>, usize) {
    let stop = flat[i + 1..]
        .iter()
        .enumerate()
        .find_map(|(offset, item)| {
            let j = i + 1 + offset;
            let last = j == flat.len() - 1;
            match item {
                NorgASTFlat::NestableDetachedModifier { level, modifier_type, .. } => {
                    if *modifier_type != list_type || level <= start_level {
                        Some((j, j - 1, false))
                    } else if last {
                        Some((j, j + 1, true))
                    } else {
                        None
                    }
                }
                NorgASTFlat::CarryoverTag { next_object, .. } => match &**next_object {
                    NorgASTFlat::NestableDetachedModifier { level, modifier_type, .. }
                        if *modifier_type != list_type || level <= start_level =>
                    {
                        Some((j, j - 1, false))
                    }
                    NorgASTFlat::NestableDetachedModifier { .. } if last => {
                        Some((j, j + 1, true))
                    }
                    NorgASTFlat::NestableDetachedModifier { .. } => None,
                    _ => Some((j, j - 1, false)),
                },
                _ => Some((j, j - 1, false)),
            }
        });

    match stop {
        Some((j, result_i, to_end)) => {
            let end = if to_end { flat.len() } else { j };
            (stage_4_from(&flat[(i + 1)..end]), result_i)
        }
        None => (vec![], flat.len()),
    }
}

fn stage_4_from(flat: &[NorgASTFlat]) -> Vec<NorgAST> {
    let mut ast = vec![];
    let mut i = 0;
    while i < flat.len() {
        let item = &flat[i];
        match item {
            NorgASTFlat::Heading {
                level: start_level,
                title,
                extensions,
            } => {
                let (content, new_i) = consume_heading_content(start_level, flat, i);

                ast.push(NorgAST::Heading {
                    level: *start_level,
                    title: title.to_vec(),
                    extensions: extensions.to_vec(),
                    content,
                });
                i = new_i;
            }
            NorgASTFlat::CarryoverTag {
                tag_type,
                name,
                parameters,
                next_object,
            } => {
                match *next_object.clone() {
                    NorgASTFlat::Heading {
                        level,
                        title,
                        extensions,
                    } => {
                        let (content, new_i) = consume_heading_content(&level, flat, i);
                        ast.push(NorgAST::CarryoverTag {
                            tag_type: tag_type.clone(),
                            name: name.to_vec(),
                            parameters: parameters.to_vec(),
                            next_object: Box::new(NorgAST::Heading {
                                level,
                                title,
                                extensions,
                                content,
                            }),
                        });
                        i = new_i;
                        i += 1;
                        continue;
                    }
                    NorgASTFlat::NestableDetachedModifier {
                        modifier_type,
                        level,
                        extensions,
                        content,
                    } => {
                        let (new_content, new_i) =
                            consume_nestable_detached_mod_content(&level, flat, i, modifier_type);
                        ast.push(NorgAST::CarryoverTag {
                            tag_type: tag_type.clone(),
                            name: name.to_vec(),
                            parameters: parameters.to_vec(),
                            next_object: Box::new(NorgAST::NestableDetachedModifier {
                                modifier_type,
                                level,
                                extensions,
                                text: content,
                                content: new_content,
                            }),
                        });
                        i = new_i;
                        i += 1;
                        continue;
                    }
                    _ => {
                        ast.push(convert(item.clone()))
                    }
                }
            }
            NorgASTFlat::NestableDetachedModifier {
                level: start_level,
                modifier_type,
                extensions,
                content: text,
            } => {
                let (content, new_i) = consume_nestable_detached_mod_content(start_level, flat, i, *modifier_type);
                let parsed = NorgAST::NestableDetachedModifier {
                    modifier_type: *modifier_type,
                    level: *start_level,
                    extensions: extensions.to_vec(),
                    text: text.clone(),
                    content,
                };
                i = new_i;
                if let Some(NorgAST::List { modifier_type: list_modifier, items }) = ast.last_mut() {
                    if *modifier_type == *list_modifier {
                        items.push(parsed);
                        i += 1;
                        continue
                    }
                }
                ast.push(NorgAST::List { modifier_type: *modifier_type, items: vec![parsed] });
            }
            _ => {
                ast.push(convert(item.clone()));
            }
        }

        i += 1;
    }

    ast
}

pub fn stage_4(flat: Vec<NorgASTFlat>) -> Vec<NorgAST> {
    stage_4_from(&flat)
}
