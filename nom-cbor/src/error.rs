use ciborium::value::Value;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error<Input> {
    /// Empty CDDL file. A CDDL file must contain _at least_ one rule.
    #[error("Does not contain any rule")]
    EmptyCddl,

    #[error("An error happened while parsing: {0}")]
    ParseError(nom::Err<nom::error::Error<Input>>),

    #[error("Cddl did not parse entirely. Remaining: {0:?}")]
    DidNotParseEntirely(Input),

    #[error("CBOR did not validate properly. Location: {0}, Value: {1:?}")]
    ValidationError(String, Value),

    #[error("A CDDL rule was not found: {0}")]
    RuleNotFound(String),

    #[error("Provided CBOR is invalid: {0:?}")]
    InvalidCbor(ciborium::de::Error<std::io::Error>),
}
