use chumsky::Parser;
pub use stage_1::NorgMeta;

use crate::error::NorgParseError;

pub mod stage_1;

/// Parses the given input string to produce an AST for the metadata
pub fn parse_metadata(input: &str) -> Result<NorgMeta, NorgParseError> {
    let processed = format!("{{\n{}\n}}\n", input.trim());
    Ok(stage_1::meta_parser().parse(processed)?)
}

#[cfg(test)]
mod tests {
    use insta::assert_yaml_snapshot;
    use itertools::Itertools;

    use crate::metadata::parse_metadata;

    #[test]
    fn common_metadata() {
        let examples: Vec<_> = [
            "
            title: Sunday November 17, 2024
            description: We Cooked
            authors: benlubas
            categories: journal
            created: 2024-11-18
            updated: 2024-11-18T17:58:21-0500
            version: 1.1.1
            ",
            "
            title: Neorg Extras
            description: Extra lua code to configure Neorg
            authors: benlubas
            categories: [
              neorg
              nvim
              config
            ]
            tangle: {
              languages: {
                lua: ~/github/.dotfiles/nvim/lua/benlubas/neorg/extras.lua
              }
              delimiter: heading
            }
            created: 2024-05-03T13:36:42-0500
            updated: 2024-10-27T11:12:32-0500
            version: 1.1.1
            ",
        ]
        .into_iter()
        .map(|example| example.to_string() + "\n")
        .map(|str| parse_metadata(&str))
        .try_collect()
        .unwrap();

        assert_yaml_snapshot!(examples);
    }

    #[test]
    fn arrays() {
        let examples: Vec<_> = [
            "empty_arr: []
             arr: [

             ]",
            "
            categories: [
              one
              two
              45
            ]",
            "
            arr: [
              arrays can contain everything
              5
              -5
              6.02e27
              nil
              {
                x: y
                a: [
                  b
                ]
              }
              []
              [
                hi
                hi
              ]
            ]",
            "arr:[]\na2:[\n]x: y",
        ]
        .into_iter()
        .map(|example| example.to_string() + "\n")
        .map(|str| parse_metadata(&str))
        .try_collect()
        .unwrap();

        assert_yaml_snapshot!(examples);
    }

    #[test]
    fn keys_and_values() {
        let examples: Vec<_> = [
            "key: value",
            "x:y",
            "x :y",
            "x:5",
            "x:-4",
            "str:-4b",
            "nil:nil",
            "nil:",
            "still_nil:
             x: y",
            "
            key: value with : in it
            key_2: value with: in it
            ",
            "keys: {
              in:
              objects: []
            }"
        ]
        .into_iter()
        .map(|example| example.to_string() + "\n")
        .map(|str| parse_metadata(&str))
        .try_collect()
        .unwrap();

        assert_yaml_snapshot!(examples);
    }
}
