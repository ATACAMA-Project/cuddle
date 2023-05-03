//! This is an example from the NOM recipe book. You can find it at
//! https://raw.githubusercontent.com/rust-bakery/nom/main/examples/string.rs
//! It has been adapted to return a string reference and defer the parsing.
//! A parsers is available in `cddl/string.rs`.
use super::InnerInputTrait;
use nom::branch::alt;
use nom::bytes::streaming::{is_not, take_while_m_n};
use nom::character::streaming::{char, multispace1};
use nom::combinator::{map, map_opt, map_res, recognize, value};
use nom::error::{FromExternalError, ParseError};
use nom::multi::{fold_many0, many0_count};
use nom::sequence::{delimited, preceded};
use nom::IResult;

/// Parse a unicode sequence, of the form u{XXXX}, where XXXX is 1 to 6
/// hexadecimal numerals. We will combine this later with parse_escaped_char
/// to parse sequences like \u{00AC}.
fn parse_unicode<Input, E>(input: Input) -> IResult<Input, char, E>
where
    Input: InnerInputTrait,
    E: ParseError<Input> + FromExternalError<Input, std::num::ParseIntError>,
{
    // `take_while_m_n` parses between `m` and `n` bytes (inclusive) that match
    // a predicate. `parse_hex` here parses between 1 and 6 hexadecimal numerals.
    let parse_hex = take_while_m_n(1, 6, |c: char| c.is_ascii_hexdigit());

    // `preceded` takes a prefix parsers, and if it succeeds, returns the result
    // of the body parsers. In this case, it parses u{XXXX}.
    let parse_delimited_hex = preceded(
        char('u'),
        // `delimited` is like `preceded`, but it parses both a prefix and a suffix.
        // It returns the result of the middle parsers. In this case, it parses
        // {XXXX}, where XXXX is 1 to 6 hex numerals, and returns XXXX
        delimited(char('{'), parse_hex, char('}')),
    );

    // `map_res` takes the result of a parsers and applies a function that returns
    // a Result. In this case we take the hex bytes from parse_hex and attempt to
    // convert them to an u32.
    let parse_u32 = map_res(parse_delimited_hex, move |hex: Input| {
        u32::from_str_radix(hex.as_ref(), 16)
    });

    // map_opt is like map_res, but it takes an Option instead of a Result. If
    // the function returns None, map_opt returns an error. In this case, because
    // not all u32 values are valid unicode code points, we have to fallibly
    // convert to char with from_u32.
    map_opt(parse_u32, std::char::from_u32)(input)
}

/// Parse an escaped character: \n, \t, \r, \u{00AC}, etc.
fn parse_escaped_char<Input, E>(input: Input) -> IResult<Input, char, E>
where
    Input: InnerInputTrait,
    E: ParseError<Input> + FromExternalError<Input, std::num::ParseIntError>,
{
    preceded(
        char('\\'),
        // `alt` tries each parsers in sequence, returning the result of
        // the first successful match
        alt((
            parse_unicode,
            // The `value` parsers returns a fixed value (the first argument) if its
            // parsers (the second argument) succeeds. In these cases, it looks for
            // the marker characters (n, r, t, etc) and returns the matching
            // character (\n, \r, \t, etc).
            value('\n', char('n')),
            value('\r', char('r')),
            value('\t', char('t')),
            value('\u{08}', char('b')),
            value('\u{0C}', char('f')),
            value('\\', char('\\')),
            value('/', char('/')),
            value('"', char('"')),
        )),
    )(input)
}

/// Parse a backslash, followed by any amount of whitespace. This is used later
/// to discard any escaped whitespace.
fn parse_escaped_whitespace<Input, E: ParseError<Input>>(input: Input) -> IResult<Input, Input, E>
where
    Input: InnerInputTrait,
{
    preceded(char('\\'), multispace1)(input)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum QuoteType {
    Single,
    Double,
}

/// Parse a non-empty block of text that doesn't include \ or "
fn parse_literal<Input, E: ParseError<Input>>(
    q: QuoteType,
) -> impl FnMut(Input) -> IResult<Input, Input, E>
where
    Input: InnerInputTrait,
{
    move |input: Input| match match q {
        QuoteType::Double => is_not("\"\\"),
        QuoteType::Single => is_not("'\\"),
    }(input.clone())
    {
        Err(nom::Err::Incomplete(_)) => Ok((input.slice(input.input_len()..), input)),
        rest => rest,
    }
}

/// A string fragment contains a fragment of a string being parsed: either
/// a non-empty Literal (a series of non-escaped characters), a single
/// parsed escaped character, or a block of escaped whitespace.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum StringFragment<Input>
where
    Input: InnerInputTrait,
{
    Literal(Input),
    EscapedChar(char),
    EscapedWS,
}

/// Combine parse_literal, parse_escaped_whitespace, and parse_escaped_char
/// into a StringFragment.
fn parse_fragment<Input, E>(
    q: QuoteType,
) -> impl FnMut(Input) -> IResult<Input, StringFragment<Input>, E>
where
    Input: InnerInputTrait,
    E: ParseError<Input> + FromExternalError<Input, std::num::ParseIntError>,
{
    move |input: Input| {
        if input.input_len() == 0 {
            return Err(nom::Err::Error(nom::error::make_error(
                input,
                nom::error::ErrorKind::Eof,
            )));
        }

        alt((
            map(parse_literal(q), StringFragment::Literal),
            map(parse_escaped_char, StringFragment::EscapedChar),
            value(StringFragment::EscapedWS, parse_escaped_whitespace),
        ))(input)
    }
}

/// Parse the inner part of a string, resolving unicode characters and special
/// characters. The inner part is the bytes between quotes (e.g. `"abc"` has
/// `abc` as the inner part).
pub(crate) fn parse_string_inner<Input, E>(
    quotes: QuoteType,
) -> impl FnMut(Input) -> IResult<Input, String, E>
where
    Input: InnerInputTrait,
    E: ParseError<Input> + FromExternalError<Input, std::num::ParseIntError>,
{
    fold_many0(
        // Our parsers function– parses a single string fragment
        parse_fragment(quotes),
        // Our init value, an empty string
        String::new,
        // Our folding function. For each fragment, append the fragment to the
        // string.
        |mut string, fragment: StringFragment<Input>| {
            match fragment {
                StringFragment::Literal(s) => string.push_str(s.as_ref()),
                StringFragment::EscapedChar(c) => string.push(c),
                StringFragment::EscapedWS => {}
            }
            string
        },
    )
}

/// Parse a string. Use a loop of parse_fragment and push all of the fragments
/// into an output string.
pub fn parse_string<Input, E>(input: Input) -> IResult<Input, String, E>
where
    Input: InnerInputTrait,
    E: ParseError<Input> + FromExternalError<Input, std::num::ParseIntError>,
{
    // Finally, parse the string. Note that, if `build_string` could accept a raw
    // " character, the closing delimiter " would never match. When using
    // `delimited` with a looping parsers (like fold), be sure that the
    // loop won't accidentally match your closing delimiter!
    delimited(char('"'), parse_string_inner(QuoteType::Double), char('"'))(input)
}

/// Parse a string and return the input as a reference.
pub fn parse_string_ref<Input, E>(input: Input) -> IResult<Input, Input, E>
where
    Input: InnerInputTrait,
    E: ParseError<Input> + FromExternalError<Input, std::num::ParseIntError>,
{
    let build_string = recognize(many0_count(parse_fragment(QuoteType::Double)));

    // Finally, parse the string. Note that, if `build_string` could accept a raw
    // " character, the closing delimiter " would never match. When using
    // `delimited` with a looping parsers (like fold), be sure that the
    // loop won't accidentally match your closing delimiter!
    delimited(char('"'), build_string, char('"'))(input)
}

/// Parse a byte string and return the input as a reference.
pub fn parse_bstring_ref<Input, E>(input: Input) -> IResult<Input, Input, E>
where
    Input: InnerInputTrait,
    E: ParseError<Input> + FromExternalError<Input, std::num::ParseIntError>,
{
    let build_bstring = recognize(many0_count(parse_fragment(QuoteType::Single)));

    // Finally, parse the string. Note that, if `build_string` could accept a raw
    // " character, the closing delimiter " would never match. When using
    // `delimited` with a looping parsers (like fold), be sure that the
    // loop won't accidentally match your closing delimiter!
    delimited(char('\''), build_bstring, char('\''))(input)
}

#[cfg(test)]
mod tests {
    #[test]
    fn parse_string_ref() {
        let input = r#""hello world""#;
        let expected = "hello world";
        let (rest, output) =
            super::parse_string_ref::<&str, nom::error::Error<&str>>(input).unwrap();
        assert_eq!(output, expected);
        assert_eq!(rest, "");
    }

    #[test]
    fn parse_string_ref_1() {
        let input = r#""hello""#;
        let expected = "hello";
        let (rest, output) =
            super::parse_string_ref::<&str, nom::error::Error<&str>>(input).unwrap();
        assert_eq!(output, expected);
        assert_eq!(rest, "");
    }

    #[test]
    fn parse_bstring_ref() {
        let input = r#"'hello world'"#;
        let expected = "hello world";
        let (rest, output) =
            super::parse_bstring_ref::<&str, nom::error::Error<&str>>(input).unwrap();
        assert_eq!(output, expected);
        assert_eq!(rest, "");
    }

    #[test]
    fn parse_bstring_ref_1() {
        let input = r#"'hello'"#;
        let expected = "hello";
        let (rest, output) =
            super::parse_bstring_ref::<&str, nom::error::Error<&str>>(input).unwrap();
        assert_eq!(output, expected);
        assert_eq!(rest, "");
    }

    #[test]
    fn parse_string_inner() {
        let input = r#"hello world"#;
        let expected = "hello world";
        let (rest, output) = super::parse_string_inner::<&str, nom::error::Error<&str>>(
            super::QuoteType::Double,
        )(input)
        .unwrap();
        assert_eq!(output, expected);
        assert_eq!(rest, "");
    }
}
