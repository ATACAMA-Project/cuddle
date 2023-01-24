use crate::cddl::ValidateContext;
use crate::parser::{Input, NomError};
use ciborium::value::Value;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error<'a> {
    /// Empty CDDL file. A CDDL file must contain _at least_ one rule.
    #[error("Does not contain any rule")]
    EmptyCddl,

    #[error("An error happened while parsing: {0}")]
    ParseError(nom::Err<NomError<'a>>),

    #[error("Cddl did not parse entirely. Remaining: {0:?}")]
    DidNotParseEntirely(Input<'a>),

    #[error("CBOR did not validate properly. Location: {0}, Value: {1:?}")]
    ValidationError(String, Value),

    #[error("A CDDL rule was not found: {0}")]
    RuleNotFound(&'a str),
}

impl<'a> Error<'a> {
    pub fn validation_error(value: &Value, context: &ValidateContext) -> Self {
        Self::ValidationError(context.location(), value.clone())
    }
}
