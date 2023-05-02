//! This module contains very low level data structures that represents the
//! AST. To parse these types you need to use the `parsers::parse_*` functions.
use crate::parsers::string::parse_string_inner;
use crate::parsers::InnerInputTrait;
use ciborium::value::{Integer, Value as CborValue};
use num_traits::Num;
use std::num::ParseIntError;

/// A single, continuous whitespace or comment.
/// In the case of the comment, this includes the end-of-line token(s) in the
/// structure. The Whitespace variant may also include CRLF in it.
///
/// A text like the following: `   \n   ; comment\n \n ` would translate
/// into 3 SingleWhitespace instances;
/// Whitespace(`   \n   `), Comment(` comment`, `\n`), Whitespace(` \n `)
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum SingleWhitespace<Input> {
    Comment(Input, Input),
    Whitespace(Input),
}

/// Multiple lines of white spaces and/or comments.
/// ```
/// # use cuddle::ast::SingleWhitespace;
/// use cuddle::ast::SingleWhitespace::{Comment, Whitespace};
/// let source = "   \n   ; comment\n \n ";
/// let (rest, result) = cuddle::parser::parse_s(source.into()).unwrap();
/// assert_eq!(rest.to_string(), "");
/// assert_eq!(
///     result.0.into_iter().map(|e| match e {
///         Comment(a, b) => format!("Comment({a}, {b})"),
///         Whitespace(a) => format!("Whitespace({a})"),
///     }).collect::<Vec<String>>(),
///     &[
///         "Whitespace(   \n   )",
///         "Comment( comment, \n)",
///         "Whitespace( \n )",
///     ],
/// )
/// ```
#[derive(Debug, Clone)]
pub struct Whitespace<Input>(pub Vec<SingleWhitespace<Input>>);

/// A CBOR value, which should be parseable directly into a [ciborium::value::Value].
/// We only keep the input reference and will parse it as value in a later step,
/// when doing resolution.
/// This speeds up parsing without affecting performance of the other phases.
#[derive(Debug, Clone)]
pub enum Value<T> {
    Boolean(bool),
    Number {
        span: T,
        sign: bool,
        int: Uint<T>,
        frac: Option<T>,
        exp: Option<(bool, T)>,
    },
    HexFloat {
        span: T,
        sign: bool,
        int: T,
        frac: Option<T>,
        exp: (bool, T),
    },
    Text(T),
    Bytes(T),
    HexBytes(T),
    Base64Bytes(T),
    Array {
        indeterminate: bool,
        inner: Vec<Value<T>>,
    },
    Map {
        indeterminate: bool,
        pairs: Vec<(Value<T>, Value<T>)>,
    },
}

impl<Input> TryInto<CborValue> for Value<Input>
where
    Input: InnerInputTrait,
{
    type Error = String;

    fn try_into(self) -> Result<CborValue, Self::Error> {
        match self {
            Value::Boolean(b) => Ok(CborValue::Bool(b)),

            // Simple number, no exponent or fractional parts.
            Value::Number {
                sign,
                int,
                exp: None,
                frac: None,
                ..
            } => {
                let i: u64 = int.try_into().map_err(|e: ParseIntError| e.to_string())?;
                Ok(CborValue::Integer(
                    Integer::try_from(if sign { -(i as i128) } else { i as i128 })
                        .map_err(|e| e.to_string())?,
                ))
            }

            Value::Number {
                span,
                sign,
                int,
                exp: Some((true, exp)),
                frac: None,
            } => {
                // Try first as an integer, else use stdlib float parser instead.
                let i: u64 = int.try_into().map_err(|e: ParseIntError| e.to_string())?;
                let exp: u32 = exp.as_ref().parse::<u32>().map_err(|e| e.to_string())?;
                if let Some(v) = (|| i.checked_mul(10u64.checked_pow(exp)?))() {
                    return Ok(CborValue::Integer(Integer::from(v)));
                }

                f64::from_str_radix(span.as_ref(), 10)
                    .map_err(|e| e.to_string())
                    .map(|f| CborValue::Float(f))
            }
            Value::Number { span, .. } => f64::from_str_radix(span.as_ref(), 10)
                .map_err(|e| e.to_string())
                .map(|f| CborValue::Float(f)),
            Value::HexFloat { sign, int, .. } => todo!(),
            Value::Text(inner) => Ok(CborValue::Text(
                parse_string_inner(inner)
                    .map_err(|e: nom::Err<nom::error::Error<Input>>| e.to_string())?
                    .1,
            )),
            Value::Bytes(inner) => {
                eprintln!("{}", inner.as_ref());
                Ok(CborValue::Bytes(
                    parse_string_inner(inner)
                        .map_err(|e: nom::Err<nom::error::Error<Input>>| e.to_string())?
                        .1
                        .into_bytes(),
                ))
            }
            Value::HexBytes(inner) => Ok(CborValue::Bytes(
                (0..inner.as_ref().len())
                    .step_by(2)
                    .map(|i| u8::from_str_radix(inner.slice(i..(i + 2)).as_ref(), 16))
                    .collect::<Result<Vec<u8>, _>>()
                    .map_err(|e| e.to_string())?,
            )),
            Value::Base64Bytes(inner) => {
                use base64::engine::{Engine, GeneralPurpose, GeneralPurposeConfig};

                Ok(CborValue::Bytes(
                    GeneralPurpose::new(&base64::alphabet::URL_SAFE, GeneralPurposeConfig::new())
                        .decode(inner.as_ref())
                        .map_err(|e| e.to_string())?,
                ))
            }
            Value::Array {
                indeterminate,
                inner,
            } => todo!(),

            // TODO: Add support for indeterminate maps.
            Value::Map {
                // indeterminate,
                pairs,
                ..
            } => Ok(CborValue::Map(
                pairs
                    .into_iter()
                    .map(|(x, y)| {
                        let x: CborValue = x.try_into()?;
                        let y: CborValue = y.try_into()?;
                        Ok((x, y))
                    })
                    .collect::<Result<_, Self::Error>>()?,
            )),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Uint<T> {
    Uint(T),
    HexUint(T),
    OctUint(T),
    BinUint(T),
}

impl<T> TryInto<u64> for Uint<T>
where
    T: AsRef<str>,
{
    type Error = ParseIntError;

    fn try_into(self) -> Result<u64, Self::Error> {
        match self {
            Uint::Uint(i) => i.as_ref().parse(),
            Uint::HexUint(i) => todo!(),
            Uint::OctUint(i) => todo!(),
            Uint::BinUint(i) => todo!(),
        }
    }
}
