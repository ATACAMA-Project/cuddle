//! This module contains very low level data structures that represents the
//! AST. To parse these types you need to use the `parser::parse_*` functions.
use crate::error::Error;
use crate::parser::Input;
use either::Either;

/// A single, continuous whitespace or comment.
/// In the case of the comment, this includes the end-of-line token(s) in the
/// structure. The Whitespace variant may also include CRLF in it.
///
/// A text like the following: `   \n   ; comment\n \n ` would translate
/// into 3 SingleWhitespace instances;
/// Whitespace(`   \n   `), Comment(` comment`, `\n`), Whitespace(` \n `)
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum SingleWhitespace<'a> {
    Comment(Input<'a>, Input<'a>),
    Whitespace(Input<'a>),
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
pub struct Whitespace<'a>(pub Vec<SingleWhitespace<'a>>);

#[derive(Debug, Clone)]
pub enum Assignt {
    Equal,
    AdditionalEqual,
}

#[derive(Debug, Clone)]
pub enum Assigng {
    Equal,
    AdditionalEqual,
}

#[derive(Debug, Clone)]
pub enum Rule<'a> {
    Typename(
        Identifier<'a>,
        Option<GenericParm<'a>>,
        Whitespace<'a>,
        Assignt,
        Whitespace<'a>,
        Type<'a>,
    ),
    Groupname(
        Identifier<'a>,
        Option<GenericParm<'a>>,
        Whitespace<'a>,
        Assigng,
        Whitespace<'a>,
        GroupEnt<'a>,
    ),
}

#[derive(Debug, Clone)]
pub struct Identifier<'a>(pub Input<'a>);

#[derive(Debug, Clone)]
pub struct GenericParm<'a> {
    pub identifiers: Vec<(Whitespace<'a>, Identifier<'a>, Whitespace<'a>)>,
}

#[derive(Debug, Clone)]
pub struct GenericArg<'a> {
    pub types: Vec<(Whitespace<'a>, Type1<'a>, Whitespace<'a>)>,
}

#[derive(Debug, Clone)]
pub struct Type<'a> {
    pub first: Type1<'a>,
    pub types: Vec<(Whitespace<'a>, Whitespace<'a>, Type1<'a>)>,
}

#[derive(Debug, Clone)]
pub struct Type1<'a> {
    pub type2: Type2<'a>,
    pub op: Option<(
        Whitespace<'a>,
        Either<RangeOp, CtlOp<'a>>,
        Whitespace<'a>,
        Type2<'a>,
    )>,
}

#[derive(Debug, Clone)]
pub enum RangeOp {
    Included,
    Excluded,
}

#[derive(Debug, Clone)]
pub struct CtlOp<'a>(pub Identifier<'a>);

/// A CBOR value, which should be parseable directly into a [ciborium::value::Value].
/// We only keep the input reference and will parse it as value in a later step,
/// when doing resolution.
/// This speeds up parsing without affecting performance of the other phases.
#[derive(Debug, Clone)]
pub enum Value<'a> {
    Number(
        bool,
        Uint<'a>,
        Option<Input<'a>>,
        Option<(Option<Input<'a>>, Input<'a>)>,
    ),
    HexFloat(
        bool,
        Input<'a>,
        Option<Input<'a>>,
        (Option<Input<'a>>, Input<'a>),
    ),
    Text(Input<'a>),
    Bytes(Input<'a>),
    HexBytes(Input<'a>),
    Base64Bytes(Input<'a>),
}

#[derive(Debug, Clone)]
pub enum Uint<'a> {
    Uint(Input<'a>),
    HexUint(Input<'a>),
    OctUint(Input<'a>),
    BinUint(Input<'a>),
}

#[derive(Debug, Clone)]
pub enum Occur<'a> {
    Many0,
    Many1,
    RangeFrom(Uint<'a>),
    RangeTo(Uint<'a>),
    RangeFromTo(Uint<'a>, Uint<'a>),
    Optional,
}

#[derive(Debug, Clone)]
pub enum Memberkey<'a> {
    MapsTo(Type1<'a>, Whitespace<'a>, Option<Whitespace<'a>>),
    Bareword(Identifier<'a>, Whitespace<'a>),
    Value(Value<'a>, Whitespace<'a>),
}

#[derive(Debug, Clone)]
pub enum GroupEnt<'a> {
    Type(
        Option<(Occur<'a>, Whitespace<'a>)>,
        Option<(Memberkey<'a>, Whitespace<'a>)>,
        Type<'a>,
    ),
    Bareword(
        Option<(Occur<'a>, Whitespace<'a>)>,
        Identifier<'a>,
        Option<GenericArg<'a>>,
    ),
    Group(
        Option<(Occur<'a>, Whitespace<'a>)>,
        Whitespace<'a>,
        Box<Group<'a>>,
        Whitespace<'a>,
    ),
}

#[derive(Debug, Clone)]
pub struct GroupChoice<'a>(pub Vec<(GroupEnt<'a>, Whitespace<'a>, Option<Whitespace<'a>>)>);

#[derive(Debug, Clone)]
pub struct Group<'a> {
    pub first: GroupChoice<'a>,
    pub types: Vec<(Whitespace<'a>, Whitespace<'a>, GroupChoice<'a>)>,
}

#[derive(Debug, Clone)]
pub enum Type2<'a> {
    Value(Value<'a>),
    Typename(Identifier<'a>, Option<GenericArg<'a>>),
    Parenthesis(Whitespace<'a>, Box<Type<'a>>, Whitespace<'a>),
    Record(Whitespace<'a>, Group<'a>, Whitespace<'a>),
    Array(Whitespace<'a>, Group<'a>, Whitespace<'a>),
    Unwrap(Whitespace<'a>, Group<'a>, Option<GenericArg<'a>>),
    ChoiceParen(Whitespace<'a>, Whitespace<'a>, Group<'a>, Whitespace<'a>),
    Choice(Whitespace<'a>, Identifier<'a>, Option<GenericArg<'a>>),
    Tagged(
        Option<Uint<'a>>,
        Whitespace<'a>,
        Box<Type<'a>>,
        Whitespace<'a>,
    ),
    Major(Input<'a>, Option<Uint<'a>>),
    Any,
}

#[derive(Debug, Clone)]
pub struct CddlRoot<'a>(
    pub String,
    pub Whitespace<'a>,
    pub Vec<(Rule<'a>, Whitespace<'a>)>,
);

pub fn parse_cddl<'a>(cddl: &'a str, filename: &'a str) -> Result<CddlRoot<'a>, Error<'a>> {
    let fname = filename.to_string();
    let input = Input::new_extra(cddl.into(), filename);
    let (rest, (header, roots)) = crate::parser::parse_cddl(input).map_err(Error::ParseError)?;

    // This should never happen as the rules check for eof, but you never know.
    if !rest.is_empty() {
        return Err(Error::DidNotParseEntirely(rest));
    }

    Ok(CddlRoot(fname, header, roots))
}
