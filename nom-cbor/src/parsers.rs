//! Collection of [nom] parsers for CDDL.
//!
//! This follows the ABNF structure found in the
//! [RFC 8610, appendix B](https://www.rfc-editor.org/rfc/rfc8610.html#appendix-B).
use crate::ast::{SingleWhitespace, Uint, Value, Whitespace};
use ciborium::value::{Integer, Value as CborValue};
use nom::branch::alt;
use nom::bytes::complete::{tag, take_while};
use nom::character::complete::{
    char, digit0, digit1, hex_digit1, line_ending, multispace1, not_line_ending, oct_digit1,
    one_of, satisfy,
};
use nom::combinator::{map, opt, recognize, value};
use nom::multi::{many0, many0_count, separated_list0};
use nom::sequence::{delimited, pair, preceded, terminated, tuple};
use nom::{
    AsChar, Compare, IResult, InputIter, InputLength, InputTake, InputTakeAtPosition, Offset, Slice,
};
use std::fmt::Debug;
use std::ops::{Range, RangeFrom, RangeTo};

pub mod string;

pub type NomError<Input> = nom::error::Error<Input>;
pub type ParseResult<Input, T> = IResult<Input, T, NomError<Input>>;

/// To simplify function declarations, we use an inner trait declaration.
pub trait InnerInputTrait:
    AsRef<str>
    + Debug
    + Compare<&'static str>
    + InputIter<Item = char>
    + InputLength
    + InputTake
    + Offset
    + Slice<RangeFrom<usize>>
    + Slice<Range<usize>>
    + Slice<RangeTo<usize>>
    + InputTakeAtPosition<Item = char>
    + Clone
{
}

impl<T> InnerInputTrait for T where
    T: AsRef<str>
        + Debug
        + Compare<&'static str>
        + InputIter<Item = char>
        + InputLength
        + InputTake
        + Offset
        + Slice<Range<usize>>
        + Slice<RangeFrom<usize>>
        + Slice<RangeTo<usize>>
        + InputTakeAtPosition<Item = char>
        + Clone
{
}

/// Parse a whitespace, either whitespaces or comments.
pub fn parse_s<Input>(input: Input) -> ParseResult<Input, Whitespace<Input>>
where
    Input: InnerInputTrait,
{
    map(
        many0(alt((
            map(multispace1, SingleWhitespace::Whitespace),
            map(
                preceded(char(';'), pair(not_line_ending, line_ending)),
                |(a, b)| SingleWhitespace::Comment(a, b),
            ),
        ))),
        Whitespace,
    )(input)
}

pub fn parse_uint<Input>(input: Input) -> ParseResult<Input, Uint<Input>>
where
    Input: InnerInputTrait,
{
    alt((
        map(recognize(pair(one_of("123456789"), digit0)), Uint::Uint),
        map(preceded(tag("0x"), hex_digit1), Uint::HexUint),
        map(preceded(tag("0o"), oct_digit1), Uint::OctUint),
        map(
            preceded(tag("0b"), take_while(|c| c == '0' || c == '1')),
            Uint::BinUint,
        ),
        map(tag("0"), Uint::Uint),
    ))(input)
}

pub fn parse_int<Input>(input: Input) -> ParseResult<Input, Value<Input>>
where
    Input: InnerInputTrait,
{
    let i = input.clone();

    let (i, (sign, int, frac, exp)) = tuple((
        map(opt(tag("-")), |x| x.is_some()),
        parse_uint,
        opt(preceded(char('.'), digit1)),
        opt(preceded(
            char('e'),
            pair(
                map(
                    opt(alt((value(true, tag("+")), value(false, tag("-"))))),
                    |esign| esign != Some(false),
                ),
                digit1,
            ),
        )),
    ))(i)?;

    let index = input.offset(&i);
    let span = input.slice(..index);
    Ok((
        i,
        Value::Number {
            span,
            sign,
            int,
            frac,
            exp,
        },
    ))
}

pub fn parse_hexfloat<Input>(input: Input) -> ParseResult<Input, Value<Input>>
where
    Input: InnerInputTrait,
{
    let i = input.clone();

    let (i, (sign, _, int, frac, _, exp)) = tuple((
        map(opt(tag("-")), |x| x.is_some()),
        tag("0x"),
        hex_digit1,
        opt(preceded(char('.'), hex_digit1)),
        char('p'),
        pair(
            map(
                opt(alt((value(true, tag("+")), value(false, tag("-"))))),
                |esign| esign != Some(false),
            ),
            digit1,
        ),
    ))(i)?;

    let index = input.offset(&i);
    let span = input.slice(..index);
    Ok((
        i,
        Value::HexFloat {
            span,
            sign,
            int,
            frac,
            exp,
        },
    ))
}

pub fn parse_number<Input>(input: Input) -> ParseResult<Input, Value<Input>>
where
    Input: InnerInputTrait,
{
    alt((parse_hexfloat, parse_int))(input)
}

pub fn parse_string<Input>(input: Input) -> ParseResult<Input, Value<Input>>
where
    Input: InnerInputTrait,
{
    map(string::parse_string_ref, Value::Text)(input)
}

pub fn parse_bytes<Input>(input: Input) -> ParseResult<Input, Value<Input>>
where
    Input: InnerInputTrait,
{
    alt((
        map(
            preceded(
                char('h'),
                delimited(
                    char('\''),
                    recognize(many0_count(pair(
                        satisfy(char::is_hex_digit),
                        satisfy(char::is_hex_digit),
                    ))),
                    char('\''),
                ),
            ),
            Value::HexBytes,
        ),
        map(
            preceded(
                tag("b64"),
                delimited(
                    char('\''),
                    recognize(many0_count(one_of(
                        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_=",
                    ))),
                    char('\''),
                ),
            ),
            Value::Base64Bytes,
        ),
        map(string::parse_bstring_ref, Value::Bytes),
    ))(input)
}

pub fn parse_bool<Input>(input: Input) -> ParseResult<Input, Value<Input>>
where
    Input: InnerInputTrait,
{
    alt((
        value(Value::Boolean(true), tag("true")),
        value(Value::Boolean(false), tag("false")),
    ))(input)
}

/// Parse a direct CBOR value.
/// There is a missing part of the RFC ABNF syntax for octal parsing, but they
/// are included in the description of numbers (with examples), and as such are
/// considered part of the standard and added here (but missing from the BNF
/// below).
///
/// ```bnf
/// number = hexfloat / (int ["." fraction] ["e" exponent ])
/// int = ["-"] uint
/// uint = DIGIT1 *DIGIT
///      / "0x" 1*HEXDIG
///      / "0b" 1*BINDIG
///      / "0"
///
/// hexfloat = ["-"] "0x" 1*HEXDIG ["." 1*HEXDIG] "p" exponent
/// fraction = 1*DIGIT
/// exponent = ["+"/"-"] 1*DIGIT
///
/// text = %x22 *SCHAR %x22
/// SCHAR = %x20-21 / %x23-5B / %x5D-7E / %x80-10FFFD / SESC
/// SESC = "\" (%x20-7E / %x80-10FFFD)
///
/// bytes = [bsqual] %x27 *BCHAR %x27
/// BCHAR = %x20-26 / %x28-5B / %x5D-10FFFD / SESC / CRLF
/// bsqual = "h" / "b64"
/// value = number
///       / text
///       / bytes
/// ```
pub fn parse_value<Input>(input: Input) -> ParseResult<Input, Value<Input>>
where
    Input: InnerInputTrait,
{
    alt((parse_bool, parse_number, parse_string, parse_bytes))(input)
}

pub fn parse_map<Input>(input: Input) -> ParseResult<Input, Value<Input>>
where
    Input: InnerInputTrait,
{
    map(
        delimited(
            alt((tag("{"), tag("{_"))),
            separated_list0(
                pair(parse_s, char(',')),
                terminated(
                    map(
                        tuple((
                            parse_s,
                            parse_value_diag,
                            parse_s,
                            char(':'),
                            parse_s,
                            parse_value_diag,
                        )),
                        |(_, k, _, _, _, v)| (k, v),
                    ),
                    parse_s,
                ),
            ),
            tag("}"),
        ),
        |pairs| Value::Map {
            indeterminate: false,
            pairs,
        },
    )(input)
}

/// Parse a value, with support for maps and arrays.
pub fn parse_value_diag<Input>(input: Input) -> ParseResult<Input, Value<Input>>
where
    Input: InnerInputTrait,
{
    alt((parse_value, parse_map /*, parse_array */))(input)
}

#[cfg(test)]
fn assert_cbor(actual: &str, expected: CborValue) {
    let v = parse_value_diag(actual).unwrap();

    assert_eq!(v.0, "");
    assert_eq!(v.1.try_into(), Ok(expected));
}

#[test]
fn parse_simple() {
    assert_cbor("true", CborValue::Bool(true));
    assert_cbor("false", CborValue::Bool(false));
    assert_cbor("123", CborValue::Integer(123.into()));
    assert_cbor("0", CborValue::Integer(0.into()));
    assert_cbor("-0", CborValue::Integer(0.into()));
    assert_cbor(
        "18446744073709551615",
        CborValue::Integer(Integer::try_from(18446744073709551615 as i128).unwrap()),
    );
    assert_cbor("-123", CborValue::Integer((-123).into()));
    assert_cbor(
        "-18446744073709551615",
        CborValue::Integer(Integer::try_from(-18446744073709551615 as i128).unwrap()),
    );
    assert_cbor("1e2", CborValue::Integer(100.into()));
    assert_cbor("1e10", CborValue::Integer(10000000000u64.into()));

    assert_cbor("h'01020304'", CborValue::Bytes(vec![1, 2, 3, 4]));
    assert_cbor("'hello'", CborValue::Bytes(b"hello".to_vec()));
}
