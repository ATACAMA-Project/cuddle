//! This is an example from the NOM recipe book. You can find it at
//! https://raw.githubusercontent.com/rust-bakery/nom/main/examples/string.rs
//! It has been adapted to return a string reference and defer the parsing.
//! A parsers is available in `cddl/string.rs`.
use crate::parser::Input;
use nom::branch::alt;
use nom::bytes::streaming::{is_not, take_while_m_n};
use nom::character::streaming::{char, multispace1};
use nom::combinator::{map, map_opt, map_res, recognize, value};
use nom::error::{FromExternalError, ParseError};
use nom::multi::{fold_many0, many0_count, many1};
use nom::sequence::{delimited, preceded};
use nom::IResult;

pub use nom_cbor::parsers::string::*;

// /// Parse a string. Use a loop of parse_fragment and push all of the fragments
// /// into an output string.
// pub fn parse_string<'a, E>(input: Input<'a>) -> IResult<Input<'a>, String, E>
// where
//     E: ParseError<Input<'a>> + FromExternalError<Input<'a>, std::num::ParseIntError>,
// {
//     let build_string = fold_many0(
//         // Our parsers function– parses a single string fragment
//         parse_fragment,
//         // Our init value, an empty string
//         String::new,
//         // Our folding function. For each fragment, append the fragment to the
//         // string.
//         |mut string, fragment| {
//             match fragment {
//                 StringFragment::Literal(s) => string.push_str(s.fragment()),
//                 StringFragment::EscapedChar(c) => string.push(c),
//                 StringFragment::EscapedWS => {}
//             }
//             string
//         },
//     );
//
//     // Finally, parse the string. Note that, if `build_string` could accept a raw
//     // " character, the closing delimiter " would never match. When using
//     // `delimited` with a looping parsers (like fold), be sure that the
//     // loop won't accidentally match your closing delimiter!
//     delimited(char('"'), build_string, char('"'))(input)
// }
//
// /// Parse a string and return the input as a reference.
// pub fn parse_string_ref<'a, E>(input: Input<'a>) -> IResult<Input<'a>, Input<'a>, E>
// where
//     E: ParseError<Input<'a>> + FromExternalError<Input<'a>, std::num::ParseIntError>,
// {
//     let build_string = recognize(many0_count(parse_fragment));
//
//     // Finally, parse the string. Note that, if `build_string` could accept a raw
//     // " character, the closing delimiter " would never match. When using
//     // `delimited` with a looping parsers (like fold), be sure that the
//     // loop won't accidentally match your closing delimiter!
//     delimited(char('"'), build_string, char('"'))(input)
// }
//
// /// Parse a byte string and return the input as a reference.
// pub fn parse_bstring_ref<'a, E>(input: Input<'a>) -> IResult<Input<'a>, Input<'a>, E>
// where
//     E: ParseError<Input<'a>> + FromExternalError<Input<'a>, std::num::ParseIntError>,
// {
//     let build_string = recognize(many0_count(parse_fragment));
//
//     // Finally, parse the string. Note that, if `build_string` could accept a raw
//     // " character, the closing delimiter " would never match. When using
//     // `delimited` with a looping parsers (like fold), be sure that the
//     // loop won't accidentally match your closing delimiter!
//     delimited(char('\''), build_string, char('\''))(input)
// }
