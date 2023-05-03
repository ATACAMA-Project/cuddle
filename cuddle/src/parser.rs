//! Collection of [nom] parsers for CDDL.
//!
//! This follows the ABNF structure found in the
//! [RFC 8610, appendix B](https://www.rfc-editor.org/rfc/rfc8610.html#appendix-B).
use crate::ast::{
    Assigng, Assignt, CtlOp, GenericArg, GenericParm, Group, GroupChoice, GroupEnt, Identifier,
    Memberkey, Occur, RangeOp, Rule, SingleWhitespace, Type, Type1, Type2, Uint, Value, Whitespace,
};
use either::Either;
use nom::branch::alt;
use nom::bytes::complete::{tag, take_while};
use nom::character::complete::{
    char, digit0, digit1, hex_digit1, line_ending, multispace1, not_line_ending, oct_digit1,
    one_of, satisfy,
};
use nom::combinator::{eof, map, opt, recognize, value};
use nom::error::context;
use nom::multi::{many0, many0_count, many1, separated_list1};
use nom::sequence::{delimited, pair, preceded, separated_pair, terminated, tuple};
use nom::{AsChar, IResult};
use nom_locate::LocatedSpan;
use std::path::Path;

mod string;

pub type Input<'a> = LocatedSpan<&'a str, &'a Path>;
pub type NomError<'a> = nom::error::Error<Input<'a>>;

pub type CddlResult<'a, T> = IResult<Input<'a>, T, NomError<'a>>;

pub fn parse_s(input: Input) -> CddlResult<Whitespace> {
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

/// Parse an assignment sign for types.
///
/// ```bnf
/// assignt = "=" / "/="
/// ```
pub fn parse_assignt(input: Input) -> CddlResult<Assignt> {
    alt((
        value(Assignt::Equal, char('=')),
        value(Assignt::AdditionalEqual, tag("/=")),
    ))(input)
}

/// Parse an assignment sign for types.
///
/// ```bnf
/// assigng = "=" / "//="
/// ```
pub fn parse_assigng(input: Input) -> CddlResult<Assigng> {
    alt((
        value(Assigng::Equal, char('=')),
        value(Assigng::AdditionalEqual, tag("//=")),
    ))(input)
}

/// Parse a generic parameter.
///
/// ```bnf
/// genericparm = "<" S id S *("," S id S ) ">"
/// ```
pub fn parse_genericparm(input: Input) -> CddlResult<GenericParm> {
    map(
        tuple((
            char('<'),
            separated_list1(char(','), tuple((parse_s, parse_identifier, parse_s))),
            char('>'),
        )),
        |(_lt, identifiers, _gt)| GenericParm { identifiers },
    )(input)
}

/// Parse a generic argument.
///
/// ```bnf
/// genericarg = "<" S type1 S *("," S type1 S ) ">"
/// ```
pub fn parse_genericarg(input: Input) -> CddlResult<GenericArg> {
    map(
        tuple((
            char('<'),
            separated_list1(char(','), tuple((parse_s, parse_type1, parse_s))),
            char('>'),
        )),
        |(_lt, types, _gt)| GenericArg { types },
    )(input)
}

/// Parse the type rule.
///
/// ```bnf
/// type = type1 *(S "/" S type1)
/// ```
pub fn parse_type(input: Input) -> CddlResult<Type> {
    map(
        pair(
            parse_type1,
            many0(tuple((parse_s, char('/'), parse_s, parse_type1))),
        ),
        |first, types| Type {
            first,
            types: types.into_iter().map(|(a, _, c, d)| (a, c, d)).collect(),
        },
    )(input)
}

/// Parse the type1 syntax rule.
///
/// ```bnf
/// type1 = type2 [S (rangeop / ctlop) S type2]
/// ```
pub fn parse_type1(input: Input) -> CddlResult<Type1> {
    map(
        pair(
            parse_type2,
            opt(tuple((
                parse_s,
                alt((
                    map(parse_rangeop, Either::Left),
                    map(parse_ctlop, Either::Right),
                )),
                parse_s,
                parse_type2,
            ))),
        ),
        |(type2, op)| Type1 { type2, op },
    )(input)
}

pub fn parse_rangeop(input: Input) -> CddlResult<RangeOp> {
    alt((
        value(RangeOp::Included, tag("..")),
        value(RangeOp::Excluded, tag("...")),
    ))(input)
}

pub fn parse_ctlop(input: Input) -> CddlResult<CtlOp> {
    map(preceded(char('.'), parse_identifier), CtlOp)(input)
}

pub fn parse_uint(input: Input) -> CddlResult<Uint> {
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
pub fn parse_value(input: Input) -> CddlResult<Value> {
    fn parse_int(input: Input) -> CddlResult<Value> {
        map(
            tuple((
                opt(tag("-")),
                parse_uint,
                opt(preceded(char('.'), digit1)),
                opt(preceded(
                    char('e'),
                    pair(opt(alt((tag("+"), tag("-")))), digit1),
                )),
            )),
            |(sign, uint, fraction, exponent)| {
                Value::Number(sign.is_some(), uint, fraction, exponent)
            },
        )(input)
    }
    fn parse_hexfloat(input: Input) -> CddlResult<Value> {
        map(
            tuple((
                opt(char('-')),
                tag("0x"),
                hex_digit1,
                opt(preceded(char('.'), hex_digit1)),
                char('p'),
                pair(opt(alt((tag("+"), tag("-")))), digit1),
            )),
            |(sign, _, uint, fraction, _, exponent)| {
                Value::HexFloat(sign.is_some(), uint, fraction, exponent)
            },
        )(input)
    }
    fn parse_number(input: Input) -> CddlResult<Value> {
        alt((parse_hexfloat, parse_int))(input)
    }

    fn parse_string(input: Input) -> CddlResult<Value> {
        map(string::parse_string_ref, Value::Text)(input)
    }

    fn parse_bytes(input: Input) -> CddlResult<Value> {
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
        ))(input)
    }

    alt((parse_number, parse_string, parse_bytes))(input)
}

/// Parse the occur syntax rule.
///
/// ```bnf
/// occur = [uint] "*" [uint]
///       / "+"
///       / "?"
/// ```
pub fn parse_occur(input: Input) -> CddlResult<Occur> {
    alt((
        map(
            separated_pair(opt(parse_uint), char('*'), opt(parse_uint)),
            |(a, b)| match (a, b) {
                (None, None) => Occur::Many0,
                (Some(a), None) => Occur::RangeFrom(a),
                (None, Some(b)) => Occur::RangeTo(b),
                (Some(a), Some(b)) => Occur::RangeFromTo(a, b),
            },
        ),
        value(Occur::Many1, char('+')),
        value(Occur::Optional, char('?')),
    ))(input)
}

/// Parse a memberkey syntax rule.
///
/// ```bnf
/// memberkey = type1 S ["^" S] "=>"
///           / bareword S ":"
///           / value S ":"
/// ```
pub fn parse_memberkey(input: Input) -> CddlResult<Memberkey> {
    alt((
        map(
            terminated(
                tuple((parse_type1, parse_s, opt(preceded(char('^'), parse_s)))),
                tag("=>"),
            ),
            |(t1, w, a)| Memberkey::MapsTo(t1, w, a),
        ),
        map(
            terminated(pair(parse_identifier, parse_s), char(':')),
            |(a, b)| Memberkey::Bareword(a, b),
        ),
        map(
            terminated(pair(parse_value, parse_s), char(':')),
            |(a, b)| Memberkey::Value(a, b),
        ),
    ))(input)
}

/// Parse the grpent bnf syntax rule.
///
/// ```bnf
/// grpent = [occur S] [memberkey S] type
///        / [occur S] groupname [genericarg]  ; preempted by above
///        / [occur S] "(" S group S ")"
/// ```
pub fn parse_grpent(input: Input) -> CddlResult<GroupEnt> {
    alt((
        map(
            tuple((
                opt(pair(parse_occur, parse_s)),
                opt(pair(parse_memberkey, parse_s)),
                parse_type,
            )),
            |(a, b, c)| GroupEnt::Type(a, b, c),
        ),
        map(
            tuple((
                opt(pair(parse_occur, parse_s)),
                parse_identifier,
                opt(parse_genericarg),
            )),
            |(a, b, c)| GroupEnt::Bareword(a, b, c),
        ),
        map(
            tuple((
                opt(pair(parse_occur, parse_s)),
                char('('),
                parse_s,
                parse_group,
                parse_s,
                char(')'),
            )),
            |(a, _, b, c, d, _)| GroupEnt::Group(a, b, Box::new(c), d),
        ),
    ))(input)
}

/// Parse a grpchoice syntax rule.
///
/// ```bnf
/// grpchoice = *(grpent optcom)
/// ```
pub fn parse_grpchoice(input: Input) -> CddlResult<GroupChoice> {
    fn parse_optcom(input: Input) -> CddlResult<(Whitespace, Option<Whitespace>)> {
        pair(parse_s, opt(preceded(char(','), parse_s)))(input)
    }

    map(many0(pair(parse_grpent, parse_optcom)), |x| {
        GroupChoice(
            x.into_iter()
                .map(|(grpent, (a, b))| (grpent, a, b))
                .collect(),
        )
    })(input)
}

/// Parse the group syntax rule.
///
/// ```bnf
/// group = grpchoice *(S "//" S grpchoice)
/// ```
pub fn parse_group(input: Input) -> CddlResult<Group> {
    map(
        tuple((
            parse_grpchoice,
            many0(tuple((parse_s, tag("//"), parse_s, parse_grpchoice))),
        )),
        |(first, types)| Group {
            first,
            types: types.into_iter().map(|(a, _, c, d)| (a, c, d)).collect(),
        },
    )(input)
}

/// Parse the type2 rule.
///
/// ```bnf
/// type2 = value
///       / typename [genericarg]
///       / "(" S type S ")"
///       / "{" S group S "}"
///       / "[" S group S "]"
///       / "~" S typename [genericarg]
///       / "&" S "(" S group S ")"
///       / "&" S groupname [genericarg]
///       / "#" "6" ["." uint] "(" S type S ")"
///       / "#" DIGIT ["." uint]                ; major/ai
///       / "#"                                 ; any
/// ```
pub fn parse_type2(input: Input) -> CddlResult<Type2> {
    context(
        "type2",
        alt((
            map(parse_value, Type2::Value),
            map(
                pair(parse_identifier, opt(parse_genericarg)),
                |(id, arg)| Type2::Typename(id, arg),
            ),
            map(
                delimited(char('('), tuple((parse_s, parse_type, parse_s)), char(')')),
                |(a, b, c)| Type2::Parenthesis(a, Box::new(b), c),
            ),
            map(
                delimited(char('{'), tuple((parse_s, parse_group, parse_s)), char('}')),
                |(a, b, c)| Type2::Record(a, b, c),
            ),
            map(
                delimited(char('['), tuple((parse_s, parse_group, parse_s)), char(']')),
                |(a, b, c)| Type2::Array(a, b, c),
            ),
            map(
                preceded(
                    char('~'),
                    tuple((parse_s, parse_group, opt(parse_genericarg))),
                ),
                |(a, b, c)| Type2::Unwrap(a, b, c),
            ),
            map(
                preceded(
                    char('&'),
                    tuple((parse_s, char('('), parse_s, parse_group, parse_s, char(')'))),
                ),
                |(a, _, c, d, e, _)| Type2::ChoiceParen(a, c, d, e),
            ),
            map(
                preceded(
                    char('&'),
                    tuple((parse_s, parse_identifier, opt(parse_genericarg))),
                ),
                |(a, b, c)| Type2::Choice(a, b, c),
            ),
            map(
                preceded(
                    tag("#6"),
                    tuple((
                        opt(preceded(char('.'), parse_uint)),
                        char('('),
                        parse_s,
                        parse_type,
                        parse_s,
                        char(')'),
                    )),
                ),
                |(a, _, b, c, d, _)| Type2::Tagged(a, b, Box::new(c), d),
            ),
            map(
                preceded(
                    char('#'),
                    pair(
                        recognize(one_of("0123456789")),
                        opt(preceded(char('.'), parse_uint)),
                    ),
                ),
                |(a, b)| Type2::Major(a, b),
            ),
            value(Type2::Any, char('#')),
        )),
    )(input)
}

/// Identifier syntax is described in https://www.rfc-editor.org/rfc/rfc8610#section-3.1
///
/// ```bnf
/// id = EALPHA *(*("-" / ".") (EALPHA / DIGIT))
/// ALPHA = %x41-5A / %x61-7A
/// EALPHA = ALPHA / "@" / "_" / "$"
/// DIGIT = %x30-39
/// ```
pub fn parse_identifier(input: Input) -> CddlResult<Identifier> {
    let ealpha = alt((satisfy(char::is_alphabetic), one_of("@_$")));

    recognize(preceded(
        ealpha,
        many0_count(tuple((
            many0_count(one_of("-.")),
            alt((satisfy(char::is_alphanumeric), one_of("@_$-."))),
        ))),
    ))(input)
    .map(|(a, b)| (a, Identifier(b)))
}

/// Parse a typename assignment. This is the first part of the parse_rule ABNF.
///
/// ```bnf
/// typename [genericparm] S assignt S type
/// ```
pub fn parse_typename_assignment(input: Input) -> CddlResult<Rule> {
    map(
        tuple((
            parse_identifier,
            opt(parse_genericparm),
            parse_s,
            parse_assignt,
            parse_s,
            parse_type,
        )),
        |(identifier, genericparm, ws0, assignt, ws1, r#type)| {
            Rule::Typename(identifier, genericparm, ws0, assignt, ws1, r#type)
        },
    )(input)
}

/// Parse a groupname assignment.
///
/// ```bnf
/// groupname [genericparm] S assigng S grpent
/// ```
pub fn parse_groupname_assignment(input: Input) -> CddlResult<Rule> {
    map(
        tuple((
            parse_identifier,
            opt(parse_genericparm),
            parse_s,
            parse_assigng,
            parse_s,
            parse_grpent,
        )),
        |(a, b, c, d, e, f)| Rule::Groupname(a, b, c, d, e, f),
    )(input)
}

/// Parse a single rule.
///
/// ```bnf
/// rule = typename [genericparm] S assignt S type
///      / groupname [genericparm] S assigng S grpent
/// ```
pub fn parse_rule(input: Input) -> CddlResult<Rule> {
    alt((parse_typename_assignment, parse_groupname_assignment))(input)
}

/// Parse a CDDL file.
///
/// ```bnf
/// cddl = S 1*(rule S)
/// ```
pub fn parse_cddl(input: Input) -> CddlResult<(Whitespace, Vec<(Rule, Whitespace)>)> {
    terminated(pair(parse_s, many1(pair(parse_rule, parse_s))), eof)(input)
}
