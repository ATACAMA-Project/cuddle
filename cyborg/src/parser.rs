use crate::error::Error;
use ciborium::value::Value;
use std::collections::BTreeMap;
use std::rc::Rc;

/// A Variable to the parsers. The default extension `$` takes the inner
#[derive(Clone)]
pub enum Variable {
    /// A CBOR string that will recursively be interpreted using parse_diag.
    /// This cannot be a fragment, and invalid CBOR will lead to an error.
    Cbor(String),

    /// A CBOR Value that will be embedded as is.
    Value(Value),
}

pub trait ParseExtension {
    fn execute(&self, inner: &str, options: &ParseOptions) -> Result<Option<Value>, Error> {
        self.call(&parse_diag(inner, options)?, options)
    }

    fn call(&self, _arg: &Value, _options: &ParseOptions) -> Result<Option<Value>, Error> {
        unreachable!()
    }
}

#[derive(Clone)]
pub struct ParseOptions {
    variables: BTreeMap<String, Variable>,
    extensions: BTreeMap<String, Rc<dyn ParseExtension>>,
    filename: Option<String>,
}

impl ParseOptions {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn variables(self, variables: BTreeMap<String, Variable>) -> Self {
        Self { variables, ..self }
    }

    pub fn extensions(self, extensions: BTreeMap<String, Rc<dyn ParseExtension>>) -> Self {
        Self { extensions, ..self }
    }

    pub fn filename(self, filename: impl ToOwned<Owned = String>) -> Self {
        Self {
            filename: Some(filename.to_owned()),
            ..self
        }
    }

    pub fn with_variables(mut self, variables: BTreeMap<String, Variable>) -> Self {
        self.variables.extend(variables);
        self
    }

    pub fn with_variable(mut self, name: String, var: Variable) -> Self {
        self.variables.insert(name, var);
        self
    }

    pub fn with_extension<P: ParseExtension + 'static>(
        mut self,
        name: String,
        extension: P,
    ) -> Self {
        self.extensions.insert(name, Rc::new(extension));
        self
    }
}

pub mod extensions {
    use super::ParseExtension;
    use crate::error::Error;
    use crate::parser::{parse_diag, ParseOptions, Variable};
    use ciborium::value::Value;

    pub struct VariableReplacement;

    impl ParseExtension for VariableReplacement {
        fn execute(&self, inner: &str, options: &ParseOptions) -> Result<Option<Value>, Error> {
            match options.variables.get(inner) {
                Some(Variable::Value(v)) => Ok(Some(v.clone())),
                Some(Variable::Cbor(cbor)) => Ok(Some(parse_diag(cbor, options)?)),
                None => Err(Error::VariableNotFound(inner.to_string())),
            }
        }
    }

    pub struct BstrBoxing;

    impl ParseExtension for BstrBoxing {
        fn execute(&self, inner: &str, options: &ParseOptions) -> Result<Option<Value>, Error> {
            let inner = parse_diag(&inner, options)?;
            let mut buffer = Vec::new();
            ciborium::ser::into_writer(&inner, &mut buffer)
                .map_err(|e| Error::Unexpected(Box::new(e)))?;
            Ok(Some(Value::Bytes(buffer)))
        }
    }
}

impl Default for ParseOptions {
    fn default() -> Self {
        let extensions = BTreeMap::from_iter([
            (
                "$".to_string(),
                Rc::new(extensions::VariableReplacement) as Rc<dyn ParseExtension>,
            ),
            ("bstr".to_string(), Rc::new(extensions::BstrBoxing)),
        ]);

        Self {
            variables: Default::default(),
            extensions,
            filename: None,
        }
    }
}

/// Parse and result
pub fn parse_diag(input: &str, options: &ParseOptions) -> Result<Value, Error> {
    todo!()
}
