use crate::ast::CddlRoot;
use crate::error::Error;
use crate::parser::Input;
use crate::{ast, parse_cddl};
use ciborium::value::Value;
use lazy_static::lazy_static;
use std::collections::BTreeMap;
use std::rc::Rc;

lazy_static! {
    static ref PRELUDE: CddlRoot<'static> = prelude_();
}

fn prelude_() -> CddlRoot<'static> {
    parse_cddl(
        r#"
any = #

uint = #0
nint = #1
int = uint / nint

bstr = #2
bytes = bstr
tstr = #3
text = tstr

tdate = #6.0(tstr)
time = #6.1(number)
number = int / float
biguint = #6.2(bstr)
bignint = #6.3(bstr)
bigint = biguint / bignint
integer = int / bigint
unsigned = uint / biguint
decfrac = #6.4([e10: int, m: integer])
bigfloat = #6.5([e2: int, m: integer])
eb64url = #6.21(any)
eb64legacy = #6.22(any)
eb16 = #6.23(any)
encoded-cbor = #6.24(bstr)
uri = #6.32(tstr)
b64url = #6.33(tstr)
b64legacy = #6.34(tstr)
regexp = #6.35(tstr)
mime-message = #6.36(tstr)
cbor-any = #6.55799(any)
float16 = #7.25
float32 = #7.26
float64 = #7.27
float16-32 = float16 / float32
float32-64 = float32 / float64
float = float16-32 / float64

false = #7.20
true = #7.21
bool = false / true
nil = #7.22
null = nil
undefined = #7.23
"#,
        "prelude",
    )
    .unwrap()
}

/// A context used during validation of CBOR.
pub struct ValidateContext<'a> {
    /// Current generics being resolved. This is only valid in this context.
    /// If a new type is being used, generics should be copied and
    generics: BTreeMap<Identifier<'a>, Identifier<'a>>,

    /// The location, as a path-like format.
    location: Vec<String>,
}

impl<'a> ValidateContext<'a> {
    pub fn location(&self) -> String {
        self.location.join("/")
    }
}

/// Allow for validation of a CBOR value against a rule.
pub trait ValidateCbor {
    fn validate(&self, value: &Value, context: ValidateContext) -> Result<(), Error>;
}

/// An identifier.
pub struct Identifier<'a>(&'a str);

impl<'a> Identifier<'a> {
    pub fn new(from: &'a str) -> Self {
        Self(from.as_ref())
    }
}

impl<'a> From<&'a ast::Identifier<'a>> for Identifier<'a> {
    fn from(value: &'a ast::Identifier<'a>) -> Self {
        Self(value.0.fragment())
    }
}

/// A comment, with all endlines ignored. This can be built from a Whitespace.
pub struct Comment<'a>(Vec<&'a str>);

impl<'a> Comment<'a> {
    /// Creates a comment struct from a whitespace AST node.
    /// If `start` is true, the comment will use the beginning of the whitespace
    /// as the comment content, otherwise this will use the end.
    pub fn new(ws: &'a ast::Whitespace<'a>, start: bool) -> Self {
        fn predicate<'a>(i: &'a ast::SingleWhitespace) -> Option<&'a str> {
            match i {
                ast::SingleWhitespace::Comment(a, _) => Some(*a.fragment()),
                _ => None,
            }
        }

        if start {
            Self(ws.0.iter().map_while(predicate).collect())
        } else {
            Self(ws.0.iter().rev().map_while(predicate).collect())
        }
    }
}

impl<'a> FromIterator<Input<'a>> for Comment<'a> {
    fn from_iter<T: IntoIterator<Item = Input<'a>>>(iter: T) -> Self {
        Self(iter.into_iter().map(|ws| *ws.fragment()).collect())
    }
}

pub enum Type<'a> {
    /// A reference to a rule.
    Rule(String),
    Constant(Value),
    Record(Vec<(Box<Type<'a>>, Box<Type<'a>>)>),
    Any,

    /// Ultimately this will fail as unimplemented.
    Unimplemented_(&'a ast::Type<'a>),
}

impl<'a> Type<'a> {
    pub fn new(ast: &'a ast::Type) -> Result<Self, Error<'a>> {
        todo!()
    }
}

impl<'a> ValidateCbor for Type<'a> {
    fn validate(&self, value: &Value, context: ValidateContext) -> Result<(), Error> {
        match self {
            Type::Constant(v) => {
                if value == v {
                    Ok(())
                } else {
                    Err(Error::validation_error(value, &context))
                }
            }
            Type::Record(_) => unreachable!(),
            Type::Any => Ok(()),
            Type::Rule(_) => {
                todo!()
            }
            Type::Unimplemented_(inner) => {
                todo!("{inner:?}")
            }
        }
    }
}

struct RuleInner<'a> {
    identifier: Identifier<'a>,
    comment: Comment<'a>,
    ty: Type<'a>,
}

#[derive(Clone)]
pub struct Rule<'a> {
    inner: Rc<RuleInner<'a>>,
}

impl<'a> Rule<'a> {
    pub fn new(
        header: &'a ast::Whitespace<'a>,
        rule: &'a ast::Rule<'a>,
    ) -> Result<Self, Error<'a>> {
        let comment = Comment::new(header, false);
        let inner = match rule {
            ast::Rule::Typename(identifier, _, _, _, _, ty) => Ok(RuleInner {
                identifier: identifier.into(),
                comment,
                ty: Type::new(ty)?,
            }),
            ast::Rule::Groupname(_, _, _, _, _, _) => {
                unreachable!()
            }
        }?;
        Ok(Self {
            inner: Rc::new(inner),
        })
    }

    pub fn name(&self) -> &'a str {
        self.inner.identifier.0
    }

    pub fn is_typedecl(&self) -> bool {
        match self.inner.ty {
            Type::Rule(..) => true,
            _ => false,
        }
    }
}

/// A validated CDDL instance, ready to validate CBOR values.
/// The `ast` and `parsers` functions only check that the syntax is valid, and
/// not the rules themselves. When creating this structure, all rule names are
/// cached, ensure they are not re-assigned or point to unknown rules, and
/// that all values actually are valid.
///
/// This also creates better structures where comments are attached to the node
/// below them, ignoring whitespaces, and creating a simpler tree.
///
/// Some whitespaces and comments are lost in this structure, but spans are kept
/// and can be used to modify the original source. An example of a comment that
/// would be lost is between a typename and its assignment sign, e.g.
/// in `some-type ; comment\n; other comment\n = #`. Both comments here are
/// going to be dropped from this structure.
pub struct Cddl<'a> {
    /// Includes the top-level comment only. If there are rules without spacing
    /// to the header, this will be empty and the header comment will be linked
    /// to the first rule.
    header: Comment<'a>,

    /// The first rule name of the CDDL. CDDL specifies that the first rule
    /// should be used to validate CBOR.
    first: &'a str,

    /// The list of rules, including their comments.
    rules: BTreeMap<&'a str, Rule<'a>>,
}

impl<'a> Cddl<'a> {
    fn with_cddl_root_impl<'b: 'a>(
        mut self,
        cddl_root: &'b ast::CddlRoot<'b>,
        is_prelude: bool,
    ) -> Result<Self, Error<'b>> {
        let ast::CddlRoot(_, head, roots) = cddl_root;

        if !is_prelude {
            let mut header = Comment::new(head, true);
            self.header.0.append(&mut header.0);
        }

        let mut current: &ast::Whitespace = &head;
        let mut first = None;
        let mut rules = BTreeMap::new();

        for (rule, ws) in roots {
            let rule = Rule::new(current, rule)?;
            let name = rule.name();
            if rule.is_typedecl() && first.is_none() {
                first = Some(name);
            }

            rules.insert(name, rule);
            current = &ws;
        }

        if !is_prelude {
            if let Some(f) = first {
                self.first = f;
            }
        }

        Ok(self)
    }

    pub fn empty() -> Self {
        Self {
            header: Comment(vec![]),
            first: "",
            rules: Default::default(),
        }
    }

    pub fn with_prelude(mut self) -> Self {
        let cddl: &'static CddlRoot<'static> = &PRELUDE;
        self.with_cddl_root_impl(cddl, true).unwrap()
    }

    pub fn from_cddl_root(cddl_root: &'a ast::CddlRoot<'a>) -> Result<Self, Error> {
        Self::empty().with_cddl_root(cddl_root)
    }

    pub fn with_cddl_root(mut self, cddl_root: &'a ast::CddlRoot<'a>) -> Result<Self, Error>
    where
        Self: 'a,
    {
        self.with_cddl_root_impl(cddl_root, false)
    }

    pub fn validate_cbor(&self, cbor: impl AsRef<[u8]>) -> Result<(), Error<'a>> {
        self.validate_cbor_with(cbor, self.first)
    }
    pub fn validate_cbor_with(
        &self,
        cbor: impl AsRef<[u8]>,
        name: impl AsRef<str>,
    ) -> Result<(), Error<'a>> {
        let cbor: Value = ciborium::de::from_reader(cbor.as_ref()).map_err(Error::InvalidCbor)?;
        let name = name.as_ref();

        todo!();
    }
}
