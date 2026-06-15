use chumsky::Parser;
pub use stage_1::NorgMeta;

use crate::error::NorgParseError;

pub mod stage_1;

/// Parses the given input string to produce an AST for the metadata
pub fn parse_metadata(input: &str) -> Result<NorgMeta, NorgParseError> {
    let processed = input.replace("\n]", "\n ]");
    let processed = format!("{{\n{}\n}}\n", processed.trim());
    let result = stage_1::meta_parser().parse(processed.as_str()).into_result()
        .map_err(|e| NorgParseError::Meta(e.into_iter().map(|e| e.into_owned()).collect::<Vec<_>>().remove(0)))?;
    Ok(result)
}