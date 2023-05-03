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
