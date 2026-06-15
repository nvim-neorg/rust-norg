use chumsky::Parser as _;
use error::NorgParseError;

pub use crate::stage_1::stage_1;
pub use crate::stage_2::stage_2;
pub use crate::stage_4::stage_4;

pub use crate::stage_2::ParagraphSegmentToken;
pub use crate::stage_3::*;
pub use crate::stage_4::NorgAST;

mod error;
pub mod metadata;
mod stage_1;
mod stage_2;
mod stage_3;
mod stage_4;

/// Parses the given input string through multiple stages to produce a flattened abstract syntax tree (AST).
///
/// # Arguments
///
/// * `input` - A string slice that holds the input to be parsed.
///
/// # Returns
///
/// * `Ok(Vec<NorgASTFlat>)` if parsing is successful.
/// * `Err(NorgParseError)` if any stage of parsing fails.
pub fn parse(input: &str) -> Result<Vec<NorgASTFlat>, NorgParseError> {
    let tokens = stage_1().parse(input).into_result()
        .map_err(|e| NorgParseError::Stage1(e.into_iter().map(|e| e.into_owned()).collect()))?;
    let blocks = stage_2().parse(&tokens).into_result()
        .map_err(|e| NorgParseError::Stage2(e.into_iter().map(|e| e.into_owned()).collect()))?;
    let output = stage_3().parse(&blocks).into_result()
        .map_err(|e| NorgParseError::Stage3(e.into_iter().map(|e| e.into_owned()).collect()))?;
    Ok(output)
}

pub fn parse_tree(input: &str) -> Result<Vec<NorgAST>, NorgParseError> {
    let tokens = stage_1().parse(input).into_result()
        .map_err(|e| NorgParseError::Stage1(e.into_iter().map(|e| e.into_owned()).collect()))?;
    let blocks = stage_2().parse(&tokens).into_result()
        .map_err(|e| NorgParseError::Stage2(e.into_iter().map(|e| e.into_owned()).collect()))?;
    let flat = stage_3().parse(&blocks).into_result()
        .map_err(|e| NorgParseError::Stage3(e.into_iter().map(|e| e.into_owned()).collect()))?;
    Ok(stage_4(flat))
}

#[cfg(test)]
mod tests {
    use insta::assert_yaml_snapshot;
    use itertools::Itertools;
    use proptest::{prop_oneof, proptest};

    use crate::{parse, parse_tree};

    const TAG_NAME_REGEX: &str = r"[\w_\-\.\d]+";
    const TAG_PARAMETER_REGEX: &str = r"[^\s]+";
    const TAG_MULTI_PARAMETER_REGEX: &str = r"[^\n\r]+";

    const PARAGRAPH_REGEX: &str = r"[^[:punct:]\s][^\n\r]*";

    #[test]
    fn headings() {
        let examples: Vec<_> = [
            "* Heading",
            "********* Heading",
            "
            * Heading
              content.
            ",
            "
            ******* Heading
            ",
            "
            * Heading
            * Another heading
            ",
            "
            * Heading
            ** Subheading
            * Back to regular heading
            ",
            "
            * Heading
              sneaky content.
            ** Subheading
               more sneaky content inside.
            * Back to regular heading
            ",
        ]
        .into_iter()
        .map(|example| example.to_string() + "\n")
        .map(|str| parse(&str))
        .try_collect()
        .unwrap();

        assert_yaml_snapshot!(examples);
    }

    proptest! {
        #[test]
        fn paragraphs_proptests(paragraph_content in PARAGRAPH_REGEX) {
            parse(&paragraph_content).unwrap();
        }
    }

    #[test]
    fn modifiers() {
        let examples: Vec<_> = [
            "this *is* a test",
            "hello, *world*!",
            "*hello, world!*",
            "*hello*, world!",
            "*/hello/*, world!",
            "*hi!* how are you?",
            "this *is a test",
            "this *is/ a test",
            "this *is*/ a test",
            "this */is/*/ a test",
        ]
        .into_iter()
        .map(|example| example.to_string() + "\n")
        .map(|str| parse(&str))
        .try_collect()
        .unwrap();

        assert_yaml_snapshot!(examples);
    }

    #[test]
    fn links() {
        let examples: Vec<_> = [
            "{https://github.com/nvim-neorg/neorg}",
            "{$ hello!}",
            "{/ a-path.txt}",
            "{********* hello!}",
            "{:/some/file:*** a -path-.txt}",
            "[anchor]",
            "[anchor][description]",
            "[*anchored description*]",
            "[description]{* hello}",
            "This is a <link>!",
            "<*linkable with markup*> here!",
            "{:another_file:}",
        ]
        .into_iter()
        .map(|example| example.to_string() + "\n")
        .map(|str| parse(&str))
        .try_collect()
        .unwrap();

        assert_yaml_snapshot!(examples);
    }

    #[test]
    fn inline_verbatim() {
        let examples: Vec<_> = [
            "some text `inline verbatim`",
            "`verbatim at start`",
            "{/ some_link.txt}[with `inline verbatim` in anchor]",
            "`*markup* /inside/ /-verbatim-/`",
        ]
        .into_iter()
        .map(|example| example.to_string() + "\n")
        .map(|str| parse(&str))
        .try_collect()
        .unwrap();

        assert_yaml_snapshot!(examples);
    }
}