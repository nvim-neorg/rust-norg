use serde::Serialize;

use crate::{
    stage_2::ParagraphSegment, stage_3::NorgASTFlat, CarryoverTag, DetachedModifierExtension,
    NestableDetachedModifier, RangeableDetachedModifier,
};

#[derive(Debug, PartialEq, Eq, Clone, Hash, Serialize)]
pub enum NorgAST {
    Paragraph(ParagraphSegment),
    NestableDetachedModifier {
        modifier_type: NestableDetachedModifier,
        level: u16,
        extensions: Vec<DetachedModifierExtension>,
        text: Box<NorgASTFlat>,
        content: Vec<Self>,
    },
    RangeableDetachedModifier {
        modifier_type: RangeableDetachedModifier,
        title: ParagraphSegment,
        extensions: Vec<DetachedModifierExtension>,
        content: Vec<NorgASTFlat>,
    },
    Heading {
        level: u16,
        title: ParagraphSegment,
        extensions: Vec<DetachedModifierExtension>,
        content: Vec<Self>,
    },
    CarryoverTag {
        tag_type: CarryoverTag,
        name: Vec<String>,
        parameters: Vec<String>,
        next_object: Box<NorgASTFlat>,
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
}

fn convert(flat: NorgASTFlat) -> NorgAST {
    match flat {
        NorgASTFlat::Paragraph(tokens) => NorgAST::Paragraph(tokens),
        NorgASTFlat::NestableDetachedModifier { modifier_type, level, extensions, content } =>
        NorgAST::NestableDetachedModifier { modifier_type, level, extensions, text: content, content: vec![] },
        NorgASTFlat::RangeableDetachedModifier { modifier_type, title, extensions, content } =>
        NorgAST::RangeableDetachedModifier { modifier_type, title, extensions, content },
        NorgASTFlat::Heading { level, title, extensions } =>
        NorgAST::Heading { level, title, extensions, content: vec![] },
        NorgASTFlat::CarryoverTag { tag_type, name, parameters, next_object } =>
        NorgAST::CarryoverTag { tag_type, name, parameters, next_object },
        NorgASTFlat::VerbatimRangedTag { name, parameters, content } => NorgAST::VerbatimRangedTag { name, parameters, content },
        NorgASTFlat::RangedTag { name, parameters, content } => NorgAST::RangedTag { name, parameters, content },
        NorgASTFlat::InfirmTag { name, parameters } => NorgAST::InfirmTag { name, parameters },
    }
}

pub fn stage_4(flat: Vec<NorgASTFlat>) -> Vec<NorgAST> {
    let mut ast = vec![];
    let mut i = 0;
    while i < flat.len() {
        let item = &flat[i];
        if let NorgASTFlat::Heading { level: start_level, title, extensions } = item {
            let mut content = vec![];
            let mut seen = false;
            for j in (i+1)..flat.len() {
                if let NorgASTFlat::Heading { level, .. } = &flat[j] {
                    if level <= start_level {
                        // stop.
                        content = stage_4(flat[(i + 1)..j].to_vec());
                        i = j - 1;
                        seen = true;
                        break
                    }

                }
            }
            if !seen {
                content = stage_4(flat[i+1..].to_vec());
                i = flat.len();
            }

            ast.push(NorgAST::Heading { level: *start_level, title: title.to_vec(), extensions: extensions.to_vec(), content })
        } else if let NorgASTFlat::NestableDetachedModifier { level: start_level, modifier_type, extensions, content: text } = item {
            let mut content = vec![];
            for j in (i+1)..flat.len() {
                if let NorgASTFlat::NestableDetachedModifier { level, .. } = &flat[j] {
                    if level <= start_level {
                        // stop.
                        content = stage_4(flat[(i + 1)..j].to_vec());
                        i = j - 1;
                        break
                    }

                } else {
                    // stop immediately if we see something that's not a NestableDetachedModifier
                    // of lesser level
                    break;
                }
            }

            ast.push(NorgAST::NestableDetachedModifier {
                modifier_type: modifier_type.clone(),
                level: *start_level,
                extensions: extensions.to_vec(),
                text: text.clone(),
                content,
            });
        } else {
            ast.push(convert(item.clone()));
        }

        i += 1;
    }

    ast
}
