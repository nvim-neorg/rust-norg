use insta::{assert_ron_snapshot, with_settings};
use rust_norg::parse_tree;
use test_log::test;
use tracing::info;

// test and description pair
const ANCHOR_TESTS: [(&str, &str, &str); 16] = [
    (
        "anchor_declaration_basic",
        "Basic Anchor Declaration Tests",
        r#"[Neorg] is a fancy organizational tool.
        [Neorg]{https://github.com/nvim-neorg/neorg}"#,
    ),
    (
        "anchor_with_description",
        "check ancho declaration with description",
        r#"[anchor][custom description]. Here is [anchor]{# target-location}"#,
    ),
    (
        "anchor_definition_heading",
        "Should link anchor to heading",
        r#"[section reference]{* Important Section}"#,
    ),
    (
        "test_anchor_declaration_basic",
        "Should link anchor to external URL",
        r#"[homepage]{https://example.com}"#,
    ),
    (
        "anchor_definition_file_location",
        "anchor definition file location",
        r#"[documentation]{:docs/setup:}"#,
    ),
    (
        "multiple_anchor_declarations",
        "Multiple Anchor Usage in a same line",
        r#"Both [first] and [second] are useful resources.
        [first]{https://first.example.com}
        [second]{https://second.example.com}"#,
    ),
    (
        "repeated_anchor_usage",
        "Should link both instances to the same target",
        r#"First mention of [tool] and second mention of [tool].

[tool]{https://tool.example.com}"#,
    ),
    (
        "anchor_current_path_mapping",
        "Should generate current path URL mapping with magic mapping",
        r#"Link to [local-ref] within document.

[local-ref]{# local-reference}"#,
    ),
    (
        "anchor_in_list",
        "Should work within list items",
        r#"- First item with [list anchor]
- Second item
- Third item [list anchor]{* Target Section}"#,
    ),
    (
        "anchor_in_heading",
        "Should work within headings",
        r#"* Heading with [heading anchor] reference

[heading anchor]{https://example.com}"#,
    ),
    (
        "anchor_with_attached_modifiers",
        "Should work within attached modifiers",
        r#"This is *[important anchor]* text.

[important anchor]{# important-section}"#,
    ),
    (
        "anchor_within_attached_modifiers",
        "nested anchor inside bold para with non anchor text",
        r#"This *is [important anchor] text* "#,
    ),
    (
        "anchor_backward_reference",
        "Should handle backward references",
        r#"[early anchor]{* Early Section}

Later reference to [early anchor]."#,
    ),
    (
        "anchor_special_characters",
        "Special Characters and Edge Cases",
        r#"Link with [special & chars!] in name.

[special & chars!]{# special-target}"#,
    ),
    (
        "anchor_unicode_characters",
        "Should handle unicode characters",
        r#"Reference to [café anchor] here.

[café anchor]{https://café.example.com}"#,
    ),
    (
        "anchor_multiword_names",
        "Should handle multi-word anchor names",
        r#"Link to [multi word anchor name] reference.

[multi word anchor name]{* Target Section}"#,
    ),
];
// run  all anchor tests
#[test]
fn run_anchor_tests() {
    for (name, desc, norg_str) in ANCHOR_TESTS {
        let result = parse_tree(norg_str).expect("Failed to parse basic anchor declaration");
        info!(name, desc, "running test");
        with_settings!({
        info => &norg_str,
        description => desc,
        omit_expression => true,
    }, {assert_ron_snapshot!(name, result)});
    }
}
