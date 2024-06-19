use chumsky::Parser as _;
use error::NorgParseError;

use crate::stage_1::stage_1;
use crate::stage_2::stage_2;
use crate::stage_3::stage_3;

pub use crate::stage_2::ParagraphSegmentToken;
pub use crate::stage_3::{
    CarryoverTag, DetachedModifierExtension, NestableDetachedModifier, NorgASTFlat,
    RangeableDetachedModifier, RangedTag, TodoStatus,
};

mod error;
mod stage_1;
mod stage_2;
mod stage_3;

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
    Ok(stage_3().parse(stage_2().parse(stage_1().parse(input)?)?)?)
}

#[cfg(test)]
mod tests {
    use insta::assert_yaml_snapshot;
    use itertools::Itertools;
    use proptest::{prop_oneof, proptest};

    use crate::parse;

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

    #[test]
    fn lists() {
        let examples: Vec<_> = [
            "- Test list",
            "---- Test list",
            "
                - Test list
                - Test list
                -- Test list
                -- Test list
                - Test list
                --- Test list
            ",
            "---not list",
            // "- - a list item",
            "--> not a list",
        ]
        .into_iter()
        .map(|example| example.to_string() + "\n")
        .map(|str| parse(&str))
        .try_collect()
        .unwrap();

        assert_yaml_snapshot!(examples);
    }

    #[test]
    fn lists_regressions() {
        [
            "- - a list item",
            "---- - a list item",
            "---- > a list item",
        ]
        .into_iter()
        .map(|example| example.to_string() + "\n")
        .for_each(|str| {
            parse(&str).unwrap_err();
        });
    }

    #[test]
    fn ordered_lists() {
        let examples: Vec<_> = [
            "~ Test list",
            "~~~~ Test list",
            "
                ~ Test list
                ~ Test list
                ~~ Test list
                ~~ Test list
                ~ Test list
                ~~~ Test list
            ",
            "~~~not list",
            "~~> not a list",
        ]
        .into_iter()
        .map(|example| example.to_string() + "\n")
        .map(|str| parse(&str))
        .try_collect()
        .unwrap();

        assert_yaml_snapshot!(examples);
    }

    #[test]
    fn ordered_lists_regressions() {
        [
            "~ ~ a list item",
            "~~~~ - a list item",
            "~~~~ > a list item",
        ]
        .into_iter()
        .map(|example| example.to_string() + "\n")
        .for_each(|str| {
            parse(&str).unwrap_err();
        });
    }

    #[test]
    fn quotes() {
        let examples: Vec<_> = [
            "> Test quote",
            ">>>> Test quote",
            "
                > Test quote
                > Test quote
                >> Test quote
                >> Test quote
                > Test quote
                >>> Test quote
            ",
            ">>>not quote",
            // "> > a quote item",
            ">>- not a quote",
        ]
        .into_iter()
        .map(|example| example.to_string() + "\n")
        .map(|str| parse(&str))
        .try_collect()
        .unwrap();

        assert_yaml_snapshot!(examples);
    }

    #[test]
    fn quotes_regressions() {
        [
            "> > a list item",
            ">>>> - a list item",
            ">>>> ~ a list item",
        ]
        .into_iter()
        .map(|example| example.to_string() + "\n")
        .for_each(|str| {
            parse(&str).unwrap_err();
        });
    }

    #[test]
    fn definitions() {
        let examples: Vec<_> = [
            "$ Term
               Definition",
            "$$ Term
                Long definition
             $$",
        ]
        .into_iter()
        .map(|example| example.to_string() + "\n")
        .map(|str| parse(&str))
        .try_collect()
        .unwrap();

        assert_yaml_snapshot!(examples);
    }

    #[test]
    fn definitions_regressions() {
        [
            "$ Term Definition",
            "$$ Term
                Long definition $$",
            "$$ Term
                Long definition
             $$text",
            "$$ Term
                Long definition
             $$ text",
        ]
        .into_iter()
        .map(|example| example.to_string() + "\n")
        .for_each(|str| {
            parse(&str).unwrap_err();
        });
    }

    #[test]
    fn footnotes() {
        let examples: Vec<_> = [
            "^ Title
               Content",
            "^^ Title
                Long content
             ^^",
        ]
        .into_iter()
        .map(|example| example.to_string() + "\n")
        .map(|str| parse(&str))
        .try_collect()
        .unwrap();

        assert_yaml_snapshot!(examples);
    }

    #[test]
    fn footnotes_regressions() {
        [
            "^ Term Definition",
            "^^ Term
                Long definition ^^",
            "^^ Term
                Long definition
             ^^text",
            "^^ Term
                Long definition
             ^^ text",
        ]
        .into_iter()
        .map(|example| example.to_string() + "\n")
        .for_each(|str| {
            parse(&str).unwrap_err();
        });
    }

    #[test]
    fn tables() {
        let examples: Vec<_> = [
            ": A1
               Cell content",
            ":: A1
                Long cell content.
             ::",
        ]
        .into_iter()
        .map(|example| example.to_string() + "\n")
        .map(|str| parse(&str))
        .try_collect()
        .unwrap();

        assert_yaml_snapshot!(examples);
    }

    #[test]
    fn tables_regressions() {
        [
            ": Term Definition",
            ":: Term
                Long definition ::",
            ":: Term
                Long definition
             ::text",
            ":: Term
                Long definition
             :: text",
        ]
        .into_iter()
        .map(|example| example.to_string() + "\n")
        .for_each(|str| {
            parse(&str).unwrap_err();
        });
    }

    #[test]
    fn infirm_tags() {
        let examples: Vec<_> = [
            ".tag",
            ".tag-name_with-complexchars",
            ".tag-name_ parameter",
            ".tag-name_ one\\ large\\ parameter",
            ".tag-name_ one\\ large\\ parameter &^@! third parameter",
            ".tag.name.image https://github.com/super-special/repo.git?text=hello&other_text=bye",
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
        fn infirm_tags_proptests(tag_name in TAG_NAME_REGEX, parameter in TAG_PARAMETER_REGEX, multi_parameter in TAG_MULTI_PARAMETER_REGEX) {
            let tag = format!(".{} {} {}\n", tag_name, parameter, multi_parameter);

            // TODO: Ensure that the number of parameters parsed is correct?
            parse(&tag).unwrap();
        }
    }

    #[test]
    fn carryover_tags() {
        let examples: Vec<_> = [
            "+tag
             paragraph",
            "+tag-name_with-complexchars
             paragraph",
            "+tag-name_ parameter
             paragraph",
            "+tag-name_ one\\ large\\ parameter
             paragraph",
            "+tag-name_ one\\ large\\ parameter &^@! third parameter
             paragraph",
            "+tag.name.image https://github.com/super-special/repo.git?text=hello&other_text=bye
             paragraph",
            "#tag
             paragraph",
            "#tag-name_with-complexchars
             paragraph",
            "#tag-name_ parameter
             paragraph",
            "#tag-name_ one\\ large\\ parameter
             paragraph",
            "#tag-name_ one\\ large\\ parameter &^@! third parameter
             paragraph",
            "#tag.name.image https://github.com/super-special/repo.git?text=hello&other_text=bye
             paragraph",
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
        fn carryover_tags_proptests(tag_name in TAG_NAME_REGEX, parameter in TAG_PARAMETER_REGEX, multi_parameter in TAG_MULTI_PARAMETER_REGEX) {
            let content = format!("#{} {} {}\nhello world!", tag_name, parameter, multi_parameter);

            parse(&content).unwrap();
        }
    }

    #[test]
    fn ranged_verbatim_tags() {
        let examples: Vec<_> = [
            r#"@code
               print("Hello world!")
               @end"#,
            r#"@code.some-text.here lua\ language second-parameter
               print("Hello world!")
               @end"#,
            r#"@some-complex_tag_ first-parameter #&*(&$!) third-parameter

               function hello()
                   print("Hello World")
               end

               hello()
               @end"#,
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
        // NOTE: `.*` may at some point generate an `@end` purely by chance. There is a basic
        // check against this, but this should probably be done as a filter in proptest.
        fn ranged_verbatim_tags_proptests(tag_name in TAG_NAME_REGEX, parameter in TAG_PARAMETER_REGEX, multi_parameter in TAG_MULTI_PARAMETER_REGEX, content in ".*") {
            if content.contains("@end") {
                return Ok(());
            }

            let content = format!("@{} {} {}\n{}\n@end", tag_name, parameter, multi_parameter, content);

            parse(&content).unwrap();
        }
    }

    #[test]
    fn ranged_tags() {
        let examples: Vec<_> = [
            r#"|example
               Hello world!
               |end"#,
            r#"|example.some-text.here one\ parameter second-parameter
                #carryover
                text within
               |end"#,
            r#"|some-complex_tag_ first-parameter #&*(&$!) third-parameter
                this is some text within
               |end"#,
            r#"|example
               * Hello world!
               |end"#,
            r#"|example
               |example
               * Hello world!
               |end
               |end"#,
            r#"=example
               Hello world!
               =end"#,
            r#"=example.some-text.here one\ parameter second-parameter
                #carryover
                text within
               =end"#,
            r#"=some-complex_tag_ first-parameter #&*(&$!) third-parameter
                this is some text within
               =end"#,
            r#"=example
               * Hello world!
               =end"#,
            r#"=example
               =example
               * Hello world!
               =end
               =end"#,
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
        // NOTE: `.*` may at some point generate an `@end` purely by chance. There is a basic
        // check against this, but this should probably be done as a filter in proptest.
        fn ranged_tags_proptests(tag_type in prop_oneof!["@", "|"], tag_name in TAG_NAME_REGEX, parameter in TAG_PARAMETER_REGEX, multi_parameter in TAG_MULTI_PARAMETER_REGEX, content in PARAGRAPH_REGEX) {
            if content.contains(format!("{}end", tag_type).as_str()) {
                return Ok(());
            }

            let content = format!("{tag_type}{tag_name} {parameter} {multi_parameter}\n{content}\n{tag_type}end");

            parse(&content).unwrap();
        }
    }

    #[test]
    fn paragraphs() {
        let examples: Vec<_> = [
            "hello, world!",
            "*hello, world!*",
            "*hello,
             world!*",
            "two

             paragraphs",
            "paragraph
             here

             another paragraph
             here.",
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
        let examples: Vec<_> = ["this *is* a test", ""]
            .into_iter()
            .map(|example| example.to_string() + "\n")
            .map(|str| parse(&str))
            .try_collect()
            .unwrap();

        assert_yaml_snapshot!(examples);
    }
}
