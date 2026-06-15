use chumsky::error::Rich;

use crate::{stage_1::NorgToken, stage_2::NorgBlock};

/// Represents errors that can occur during the parsing process across different stages.
#[derive(Debug)]
pub enum NorgParseError {
    Stage1(Vec<Rich<'static, char>>),
    Stage2(Vec<Rich<'static, NorgToken>>),
    Stage3(Vec<Rich<'static, NorgBlock>>),
    Meta(Rich<'static, char>),
}

impl From<Vec<Rich<'static, char>>> for NorgParseError {
    fn from(error: Vec<Rich<'static, char>>) -> Self {
        NorgParseError::Stage1(error)
    }
}

impl From<Vec<Rich<'static, NorgToken>>> for NorgParseError {
    fn from(error: Vec<Rich<'static, NorgToken>>) -> Self {
        NorgParseError::Stage2(error)
    }
}

impl From<Vec<Rich<'static, NorgBlock>>> for NorgParseError {
    fn from(error: Vec<Rich<'static, NorgBlock>>) -> Self {
        NorgParseError::Stage3(error)
    }
}

impl From<Rich<'static, char>> for NorgParseError {
    fn from(error: Rich<'static, char>) -> Self {
        NorgParseError::Meta(error)
    }
}