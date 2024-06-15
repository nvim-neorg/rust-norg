use chumsky::Parser as _;
use crate::stage_3::stage_3;
use crate::stage_1::stage_1;
use crate::stage_2::stage_2;
use clap::Parser;
use eyre::Result;
use std::path::PathBuf;

mod stage_1;
mod stage_2;
mod stage_3;

#[derive(Parser)]
/// A compiler for the ef language.
struct Norg {
    /// The file to compile.
    file: PathBuf,
}

fn main() -> Result<()> {
    let parser = Norg::parse();

    let content = String::from_utf8(std::fs::read(parser.file)?)?;

    // println!(
    //     "{:?}",
    //     stage_1().parse_recovery(content.clone()).0.expect("Failed")
    // );

    println!("-----------------------");

    println!(
        "{:#?}",
        stage_2()
            .parse_recovery(
                stage_1()
                    .parse_recovery(content.clone())
                    .0
                    .expect("Failed stage_1")
            )
            .0
            .expect("Failed stage 2")
    );

    println!("-----------------------");

    println!(
        "{:#?}",
        stage_3()
            .parse(
                stage_2()
                    .parse(stage_1().parse_recovery(content).0.expect("Failed stage_1"))
                    .unwrap()
            )
            .expect("Failed stage 3")
    );

    Ok(())
}

// TODO(vhyrro): Create a large amount of test cases
#[cfg(test)]
mod tests {
    use chumsky::Parser;
    use insta::assert_yaml_snapshot;
    use itertools::Itertools;

    use crate::stage_3::{stage_3, NorgASTFlat};

    fn parse(content: String) -> Vec<NorgASTFlat> {
        stage_3()
            .parse(
                crate::stage_2()
                    .parse(crate::stage_1().parse(content).expect("lexing failed"))
                    .expect("block level failed"),
            )
            .expect("stage 3 failed")
    }

    #[test]
    fn headings() {
        let examples = [
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
        .map(parse)
        .collect_vec();

        assert_yaml_snapshot!(examples);
    }

    #[test]
    fn lists() {
        let examples = [
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
            "- - a list item",
            "--> not a list",
        ]
        .into_iter()
        .map(|example| example.to_string() + "\n")
        .map(parse)
        .collect_vec();

        assert_yaml_snapshot!(examples);
    }

    #[test]
    fn ordered_lists() {
        let examples = [
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
            "~ ~ a list item",
            "~~> not a list",
        ]
        .into_iter()
        .map(|example| example.to_string() + "\n")
        .map(parse)
        .collect_vec();

        assert_yaml_snapshot!(examples);
    }

    #[test]
    fn quotes() {
        let examples = [
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
            "> > a quote item",
            ">>> not a quote",
        ]
        .into_iter()
        .map(|example| example.to_string() + "\n")
        .map(parse)
        .collect_vec();

        assert_yaml_snapshot!(examples);
    }

    // TODO(vhyrro): Add regression tests too
    #[test]
    fn definitions() {
        let examples = [
            "$ Term
               Definition",
            "$$ Term
                Long definition
             $$",
        ]
        .into_iter()
        .map(|example| example.to_string() + "\n")
        .map(parse)
        .collect_vec();

        assert_yaml_snapshot!(examples);
    }

    #[test]
    fn footnotes() {
        let examples = [
            "^ Title
               Content",
            "^^ Title
                Long content
             ^^",
        ]
        .into_iter()
        .map(|example| example.to_string() + "\n")
        .map(parse)
        .collect_vec();

        assert_yaml_snapshot!(examples);
    }

    #[test]
    fn tables() {
        let examples = [
            ": A1
               Cell content",
            ":: A1
                Long cell content.
             ::",
        ]
        .into_iter()
        .map(|example| example.to_string() + "\n")
        .map(parse)
        .collect_vec();

        assert_yaml_snapshot!(examples);
    }

    // TODO(vhyrro): Maybe proptests here?
    #[test]
    fn infirm_tags() {
        let examples = [
            ".tag",
            ".tag-name_with-complexchars",
            ".tag-name_ parameter",
            ".tag-name_ one\\ large\\ parameter",
            ".tag-name_ one\\ large\\ parameter &^@! third parameter",
            ".tag.name.image https://github.com/super-special/repo.git?text=hello&other_text=bye",
        ]
        .into_iter()
        .map(|example| example.to_string() + "\n")
        .map(parse)
        .collect_vec();

        assert_yaml_snapshot!(examples);
    }

    #[test]
    fn carryover_tags() {
        let examples = [
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
        .map(parse)
        .collect_vec();

        assert_yaml_snapshot!(examples);
    }

    #[test]
    fn ranged_verbatim_tags() {
        let examples = [
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
        .map(parse)
        .collect_vec();

        assert_yaml_snapshot!(examples);
    }

    #[test]
    fn verbatim_tags() {
        let examples = [
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
        .map(parse)
        .collect_vec();

        assert_yaml_snapshot!(examples);
    }
}
