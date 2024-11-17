use chumsky::error::Simple;

use crate::{stage_1::NorgToken, stage_2::NorgBlock, NorgASTFlat};

/// Represents errors that can occur during the parsing process across different stages.
#[derive(Debug)]
pub enum NorgParseError {
    Stage1(Vec<Simple<char>>),
    Stage2(Vec<Simple<NorgToken>>),
    Stage3(Vec<Simple<NorgBlock>>),
    Stage4(Vec<Simple<NorgASTFlat>>),
    Meta(Simple<char>),
}

impl From<Vec<Simple<char>>> for NorgParseError {
    fn from(error: Vec<Simple<char>>) -> Self {
        NorgParseError::Stage1(error)
    }
}

impl From<Vec<Simple<NorgToken>>> for NorgParseError {
    fn from(error: Vec<Simple<NorgToken>>) -> Self {
        NorgParseError::Stage2(error)
    }
}

impl From<Vec<Simple<NorgBlock>>> for NorgParseError {
    fn from(error: Vec<Simple<NorgBlock>>) -> Self {
        NorgParseError::Stage3(error)
    }
}

impl From<Vec<Simple<NorgASTFlat>>> for NorgParseError {
    fn from(error: Vec<Simple<NorgASTFlat>>) -> Self {
        NorgParseError::Stage4(error)
    }
}

impl From<Simple<char>> for NorgParseError {
    fn from(error: Simple<char>) -> Self {
        NorgParseError::Meta(error)
    }
}
