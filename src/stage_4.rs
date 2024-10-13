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

fn consume_heading_content(start_level: &u16, flat: &[NorgASTFlat], i: &mut usize) -> Vec<NorgAST> {
    let mut heading_level = *start_level as i16;
    let mut content = vec![];
    let mut seen = false;
    for j in (*i + 1)..flat.len() {
        match &flat[j] {
            NorgASTFlat::Heading { level, .. } => {
                if level <= start_level {
                    // stop.
                    content = stage_4(flat[(*i + 1)..j].to_vec());
                    *i = j - 1;
                    seen = true;
                    break;
                } else {
                    heading_level = *level as i16;
                }
            }
            NorgASTFlat::DelimitingModifier(DelimitingModifier::Weak) => {
                heading_level -= 1;
                if heading_level < *start_level as i16 {
                    content = stage_4(flat[(*i + 1)..j].to_vec());
                    *i = j;
                    seen = true;
                    break;
                }
            }
            NorgASTFlat::DelimitingModifier(DelimitingModifier::Strong) => {
                content = stage_4(flat[(*i + 1)..j].to_vec());
                *i = j;
                seen = true;
                break;
            }
            NorgASTFlat::CarryoverTag { next_object, .. }
                if matches!(**next_object, NorgASTFlat::Heading { .. }) =>
            {
                if let NorgASTFlat::Heading { level, .. } = **next_object {
                    if level <= *start_level {
                        // stop.
                        content = stage_4(flat[(*i + 1)..j].to_vec());
                        *i = j - 1;
                        seen = true;
                        break;
                    } else {
                        heading_level = level as i16;
                    }
                } else {
                    unreachable!()
                }
            }
            _ => {}
        }
    }
    if !seen {
        content = stage_4(flat[*i + 1..].to_vec());
        *i = flat.len();
    }
    content
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
    i: &mut usize,
) -> Vec<NorgAST> {
    let mut content = vec![];
    for j in (*i + 1)..flat.len() {
        match &flat[j] {
            NorgASTFlat::NestableDetachedModifier { level, .. } => {
                if level <= start_level {
                    content = stage_4(flat[(*i + 1)..j].to_vec());
                    *i = j - 1;
                    break;
                } else if j == flat.len() - 1 {
                    content = stage_4(flat[(*i + 1)..].to_vec());
                    *i = j + 1;
                    break;
                }
            }
            NorgASTFlat::CarryoverTag { next_object, .. }
                if matches!(**next_object, NorgASTFlat::NestableDetachedModifier { .. }) =>
            {
                if let NorgASTFlat::NestableDetachedModifier { level, .. } = **next_object {
                    if level <= *start_level {
                        content = stage_4(flat[(*i + 1)..j].to_vec());
                        *i = j - 1;
                        break;
                    } else if j == flat.len() - 1 {
                        content = stage_4(flat[(*i + 1)..].to_vec());
                        *i = j + 1;
                        break;
                    }
                } else {
                    unreachable!()
                }
            }
            _ => {
                content = stage_4(flat[(*i + 1)..j].to_vec());
                *i = j - 1;
                // stop immediately if we see something that's not a NestableDetachedModifier
                // of lesser level
                break;
            }
        }
    }

    content
}

pub fn stage_4(flat: Vec<NorgASTFlat>) -> Vec<NorgAST> {
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
                let content = consume_heading_content(start_level, &flat, &mut i);

                ast.push(NorgAST::Heading {
                    level: *start_level,
                    title: title.to_vec(),
                    extensions: extensions.to_vec(),
                    content,
                })
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
                        let content = consume_heading_content(&level, &flat, &mut i);
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
                        })
                    }
                    NorgASTFlat::NestableDetachedModifier {
                        modifier_type,
                        level,
                        extensions,
                        content,
                    } => {
                        let new_content =
                            consume_nestable_detached_mod_content(&level, &flat, &mut i);
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
                        })
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
                let content = consume_nestable_detached_mod_content(start_level, &flat, &mut i);

                ast.push(NorgAST::NestableDetachedModifier {
                    modifier_type: modifier_type.clone(),
                    level: *start_level,
                    extensions: extensions.to_vec(),
                    text: text.clone(),
                    content,
                });
            }
            _ => {
                ast.push(convert(item.clone()));
            }
        }

        i += 1;
    }

    ast
}
