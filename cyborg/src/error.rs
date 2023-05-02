use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error(r#"Variable name "{0}" could not be found."#)]
    VariableNotFound(String),

    #[error("An error parsing the CBOR diagnostic notation occurred: {0:?}")]
    ParseError(nom::error::Error<std::io::Error>),

    #[error("An unexpected error happened: {0}")]
    Unexpected(Box<dyn std::error::Error>),
}
