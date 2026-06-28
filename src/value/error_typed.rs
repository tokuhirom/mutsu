//! `RuntimeError` constructors for declaration/syntax/typecheck errors,
//! plus JSON exception rendering (`to_json_exception`).
use super::expected_type_object;
use super::{RuntimeError, Value};
use std::collections::HashMap;

impl RuntimeError {
    /// X::Undeclared - Undeclared name
    #[allow(dead_code)]
    pub(crate) fn undeclared(what: &str, name: &str) -> Self {
        let msg = format!("Undeclared {} '{}'", what, name);
        let mut attrs = HashMap::new();
        attrs.insert("what".to_string(), Value::str(what.to_string()));
        attrs.insert("symbol".to_string(), Value::str(name.to_string()));
        attrs.insert("message".to_string(), Value::str(msg.clone()));
        Self::typed("X::Undeclared", attrs)
    }

    /// X::Undeclared::Symbols - Undeclared name (symbols variant)
    pub(crate) fn undeclared_symbols(message: impl Into<String>) -> Self {
        Self::typed_msg("X::Undeclared::Symbols", message)
    }

    /// X::Undeclared::Symbols for a routine, carrying a `routine_suggestion`
    /// hash `{ name => [close names] }` (empty list when there is no match).
    pub(crate) fn undeclared_routine_symbols(
        name: &str,
        message: impl Into<String>,
        suggestions: Vec<String>,
    ) -> Self {
        let mut attrs = HashMap::new();
        attrs.insert("message".to_string(), Value::str(message.into()));
        attrs.insert("symbol".to_string(), Value::str(name.to_string()));
        let arr: Vec<Value> = suggestions.iter().cloned().map(Value::str).collect();
        let mut map = HashMap::new();
        map.insert(name.to_string(), Value::array(arr.clone()));
        attrs.insert("routine_suggestion".to_string(), Value::hash(map));
        // Also expose a flat `suggestions` list so `.suggestions` is uniformly
        // available on X::Undeclared::Symbols (some call sites check it directly).
        attrs.insert("suggestions".to_string(), Value::array(arr));
        Self::typed("X::Undeclared::Symbols", attrs)
    }

    /// X::Undeclared::Symbols for a type/bareword, carrying a `type_suggestion`
    /// hash `{ name => [close names] }` plus a flat `suggestions` list.
    pub(crate) fn undeclared_type_symbols(
        name: &str,
        message: impl Into<String>,
        suggestions: Vec<String>,
    ) -> Self {
        let mut attrs = HashMap::new();
        attrs.insert("message".to_string(), Value::str(message.into()));
        attrs.insert("symbol".to_string(), Value::str(name.to_string()));
        let arr: Vec<Value> = suggestions.iter().cloned().map(Value::str).collect();
        let mut map = HashMap::new();
        map.insert(name.to_string(), Value::array(arr.clone()));
        attrs.insert("type_suggestion".to_string(), Value::hash(map));
        let mut post_types = HashMap::new();
        post_types.insert(name.to_string(), Value::Bool(true));
        attrs.insert("post_types".to_string(), Value::hash(post_types));
        attrs.insert("suggestions".to_string(), Value::array(arr));
        Self::typed("X::Undeclared::Symbols", attrs)
    }

    /// X::CompUnit::UnsatisfiedDependency - a required module could not be found.
    pub(crate) fn unsatisfied_dependency(module: &str) -> Self {
        let msg = format!("Could not find {} in:\n    (module repositories)", module);
        let mut attrs = HashMap::new();
        attrs.insert("specification".to_string(), Value::str(module.to_string()));
        attrs.insert("message".to_string(), Value::str(msg));
        Self::typed("X::CompUnit::UnsatisfiedDependency", attrs)
    }

    /// X::Redeclaration - Redeclared symbol
    #[allow(dead_code)]
    pub(crate) fn redeclaration(what: &str, name: &str) -> Self {
        let msg = format!("Redeclaration of {} '{}'", what, name);
        let mut attrs = HashMap::new();
        attrs.insert("what".to_string(), Value::str(what.to_string()));
        attrs.insert("symbol".to_string(), Value::str(name.to_string()));
        attrs.insert("message".to_string(), Value::str(msg.clone()));
        Self::typed("X::Redeclaration", attrs)
    }

    /// X::Redeclaration for routine - includes "multi" suggestion in message
    pub(crate) fn redeclaration_routine(name: &str) -> Self {
        let msg = format!(
            "Redeclaration of routine '{}'. Did you mean to declare a multi-sub?",
            name
        );
        let mut attrs = HashMap::new();
        attrs.insert("what".to_string(), Value::str("routine".to_string()));
        attrs.insert("symbol".to_string(), Value::str(name.to_string()));
        attrs.insert("message".to_string(), Value::str(msg.clone()));
        Self::typed("X::Redeclaration", attrs)
    }

    /// X::Method::NotFound - No such method
    #[allow(dead_code)]
    pub(crate) fn method_not_found(method: &str, typename: &str) -> Self {
        use crate::runtime::did_you_mean::{known_methods_for_type, suggest_method};

        let mut msg = format!(
            "No such method '{}' for invocant of type '{}'",
            method, typename
        );

        let candidates = known_methods_for_type(typename);
        if let Some(suggestion) = suggest_method(method, candidates) {
            msg.push_str(&format!("\nDid you mean '{}'?", suggestion));
        }

        let mut attrs = HashMap::new();
        attrs.insert("method".to_string(), Value::str(method.to_string()));
        attrs.insert("typename".to_string(), Value::str(typename.to_string()));
        attrs.insert("message".to_string(), Value::str(msg.clone()));
        Self::typed("X::Method::NotFound", attrs)
    }

    /// X::Obsolete - Obsolete syntax
    #[allow(dead_code)]
    pub(crate) fn obsolete(old: &str, replacement: &str) -> Self {
        let msg = format!(
            "Unsupported use of {}. In Raku please use: {}.",
            old, replacement
        );
        let mut attrs = HashMap::new();
        attrs.insert("old".to_string(), Value::str(old.to_string()));
        attrs.insert(
            "replacement".to_string(),
            Value::str(replacement.to_string()),
        );
        attrs.insert("message".to_string(), Value::str(msg.clone()));
        Self::typed("X::Obsolete", attrs)
    }

    /// X::Immutable - Cannot modify an immutable value
    pub(crate) fn immutable(typename: &str, method: &str) -> Self {
        let msg = format!("Cannot call '{}' on an immutable '{}'", method, typename);
        let mut attrs = HashMap::new();
        attrs.insert("typename".to_string(), Value::str(typename.to_string()));
        attrs.insert("method".to_string(), Value::str(method.to_string()));
        attrs.insert("message".to_string(), Value::str(msg.clone()));
        Self::typed("X::Immutable", attrs)
    }

    /// X::Cannot::Lazy - Cannot .elems a lazy list
    pub(crate) fn cannot_lazy(action: &str) -> Self {
        let msg = format!("Cannot .{} a lazy list", action);
        let mut attrs = HashMap::new();
        attrs.insert("action".to_string(), Value::str(action.to_string()));
        attrs.insert("message".to_string(), Value::str(msg.clone()));
        Self::typed("X::Cannot::Lazy", attrs)
    }

    /// X::Cannot::Lazy with an action string and "onto" type (e.g., for .Capture on lazy lists)
    pub(crate) fn cannot_lazy_with_action(action: &str, onto: &str) -> Self {
        let msg = format!("Cannot {} a lazy list onto a {}", action, onto);
        let mut attrs = HashMap::new();
        attrs.insert("action".to_string(), Value::str(action.to_string()));
        attrs.insert("message".to_string(), Value::str(msg.clone()));
        Self::typed("X::Cannot::Lazy", attrs)
    }

    /// X::Cannot::Lazy with a `what` attribute (e.g., for coercion to Bag/Set/Mix)
    pub(crate) fn cannot_lazy_what(what: &str) -> Self {
        let msg = format!("Cannot coerce a lazy list to a {}", what);
        let mut attrs = HashMap::new();
        attrs.insert("action".to_string(), Value::str("coerce".to_string()));
        attrs.insert("what".to_string(), Value::str(what.to_string()));
        attrs.insert("message".to_string(), Value::str(msg.clone()));
        Self::typed("X::Cannot::Lazy", attrs)
    }

    /// X::Syntax::Missing - Missing required syntax element
    #[allow(dead_code)]
    pub(crate) fn syntax_missing(what: &str) -> Self {
        let msg = format!("Missing {}", what);
        let mut attrs = HashMap::new();
        attrs.insert("what".to_string(), Value::str(what.to_string()));
        attrs.insert("message".to_string(), Value::str(msg.clone()));
        Self::typed("X::Syntax::Missing", attrs)
    }

    /// X::Syntax::Confused - Confused parse error
    #[allow(dead_code)]
    pub(crate) fn syntax_confused(message: impl Into<String>) -> Self {
        Self::typed_msg("X::Syntax::Confused", message)
    }

    /// X::Syntax::Confused with a reason attribute (for "Two terms in a row" etc.)
    pub(crate) fn syntax_confused_with_reason(reason: impl Into<String>) -> Self {
        let reason = reason.into();
        let mut attrs = HashMap::new();
        attrs.insert("message".to_string(), Value::str(reason.clone()));
        attrs.insert("reason".to_string(), Value::str(reason));
        Self::typed("X::Syntax::Confused", attrs)
    }

    /// X::Syntax::Malformed - Malformed syntax
    #[allow(dead_code)]
    pub(crate) fn syntax_malformed(what: &str, message: impl Into<String>) -> Self {
        let message = message.into();
        let mut attrs = HashMap::new();
        attrs.insert("what".to_string(), Value::str(what.to_string()));
        attrs.insert("message".to_string(), Value::str(message.clone()));
        Self::typed("X::Syntax::Malformed", attrs)
    }

    /// X::ControlFlow::Return - Return outside of routine
    pub(crate) fn controlflow_return(out_of_dynamic_scope: bool) -> Self {
        let msg = "Attempt to return outside of any Routine".to_string();
        let mut attrs = HashMap::new();
        attrs.insert("message".to_string(), Value::str(msg.clone()));
        attrs.insert(
            "out-of-dynamic-scope".to_string(),
            Value::Bool(out_of_dynamic_scope),
        );
        Self::typed("X::ControlFlow::Return", attrs)
    }

    /// X::TypeCheck::Assignment - Type check failed in assignment (with optional symbol).
    /// raku exposes `.expected` as the expected type OBJECT and `.got` as the
    /// offending VALUE; the message still names the value's type.
    pub(crate) fn typecheck_assignment(
        expected: &str,
        got_value: &Value,
        symbol: Option<&str>,
    ) -> Self {
        let got_type = crate::value::types::what_type_name(got_value);
        let msg = if let Some(sym) = symbol {
            format!(
                "Type check failed in assignment to {}; expected {}, got {}",
                sym, expected, got_type
            )
        } else {
            format!(
                "Type check failed in assignment; expected {}, got {}",
                expected, got_type
            )
        };
        let mut attrs = HashMap::new();
        attrs.insert("expected".to_string(), expected_type_object(expected));
        attrs.insert("got".to_string(), got_value.clone());
        if let Some(sym) = symbol {
            attrs.insert("symbol".to_string(), Value::str(sym.to_string()));
        }
        attrs.insert("message".to_string(), Value::str(msg.clone()));
        Self::typed("X::TypeCheck::Assignment", attrs)
    }

    /// X::Parameter::InvalidConcreteness - Invocant concreteness mismatch
    pub(crate) fn parameter_invalid_concreteness(
        expected: &str,
        got: &str,
        routine: &str,
        param: &str,
        should_be_concrete: bool,
        param_is_invocant: bool,
    ) -> Self {
        let kind = if should_be_concrete {
            "an object instance"
        } else {
            "a type object"
        };
        let actual_kind = if should_be_concrete {
            "a type object"
        } else {
            "an object instance"
        };
        let msg = format!(
            "Invocant of method '{}' must be {} of type\n'{}', not {} of type '{}'.  Did you forget a '.new'?",
            routine, kind, expected, actual_kind, got
        );
        let mut attrs = HashMap::new();
        attrs.insert("expected".to_string(), Value::str(expected.to_string()));
        attrs.insert("got".to_string(), Value::str(got.to_string()));
        attrs.insert("routine".to_string(), Value::str(routine.to_string()));
        attrs.insert("param".to_string(), Value::str(param.to_string()));
        attrs.insert(
            "should-be-concrete".to_string(),
            Value::Bool(should_be_concrete),
        );
        attrs.insert(
            "param-is-invocant".to_string(),
            Value::Bool(param_is_invocant),
        );
        attrs.insert("message".to_string(), Value::str(msg.clone()));
        Self::typed("X::Parameter::InvalidConcreteness", attrs)
    }

    /// X::IO::Closed - IO::Handle is closed
    pub(crate) fn io_closed(trying: &str) -> Self {
        let msg = format!("Cannot do '{}' on a closed handle", trying);
        let mut attrs = HashMap::new();
        attrs.insert("trying".to_string(), Value::str(trying.to_string()));
        attrs.insert("message".to_string(), Value::str(msg.clone()));
        Self::typed("X::IO::Closed", attrs)
    }

    /// X::Bind - Cannot bind to a thing
    #[allow(dead_code)]
    pub(crate) fn bind(target: &str) -> Self {
        let msg = format!("Cannot bind to {}", target);
        let mut attrs = HashMap::new();
        attrs.insert("target".to_string(), Value::str(target.to_string()));
        attrs.insert("message".to_string(), Value::str(msg.clone()));
        Self::typed("X::Bind", attrs)
    }

    /// X::IllegalDimensionInShape - Illegal dimension in shaped array declaration
    pub(crate) fn illegal_dimension_in_shape(dim: i64) -> Self {
        let msg = format!(
            "Illegal dimension in shape: {}. All dimensions must be integers bigger than 0",
            dim
        );
        let mut attrs = HashMap::new();
        attrs.insert("dim".to_string(), Value::Int(dim));
        attrs.insert("message".to_string(), Value::str(msg.clone()));
        Self::typed("X::IllegalDimensionInShape", attrs)
    }

    /// X::Adverb - Unsupported adverb combination on subscript access
    pub(crate) fn x_adverb(
        what: &str,
        source: &str,
        nogo: &[String],
        unexpected: &[String],
    ) -> Self {
        let nogo_display = nogo
            .iter()
            .map(|s| format!("'{}'", s))
            .collect::<Vec<_>>()
            .join(", ");
        let msg = format!(
            "Unsupported combination of adverbs ({}) passed to {}\non '{}'.",
            nogo_display, what, source
        );
        let mut attrs = HashMap::new();
        attrs.insert("what".to_string(), Value::str(what.to_string()));
        attrs.insert("source".to_string(), Value::str(source.to_string()));
        attrs.insert(
            "nogo".to_string(),
            Value::array(nogo.iter().map(|s| Value::str(s.to_string())).collect()),
        );
        // `.unexpected` is a list of the unknown adverb names (Raku returns a
        // Seq), so smartmatch against `<foo>` / a regex over its elements works.
        attrs.insert(
            "unexpected".to_string(),
            Value::array(
                unexpected
                    .iter()
                    .map(|s| Value::str(s.to_string()))
                    .collect(),
            ),
        );
        attrs.insert("message".to_string(), Value::str(msg.clone()));
        Self::typed("X::Adverb", attrs)
    }

    /// X::TypeCheck::Binding::Parameter - Type check failed in binding to parameter
    pub(crate) fn typecheck_binding_parameter(
        param: &str,
        expected: &str,
        got: &str,
        message: Option<String>,
    ) -> Self {
        let msg = message.unwrap_or_else(|| {
            format!(
                "X::TypeCheck::Binding::Parameter: Type check failed in binding to parameter '{}'; expected {}, got {}",
                param, expected, got
            )
        });
        let mut attrs = HashMap::new();
        attrs.insert("parameter".to_string(), Value::str(param.to_string()));
        // raku exposes `.expected` as the expected type OBJECT.
        attrs.insert("expected".to_string(), expected_type_object(expected));
        attrs.insert("got".to_string(), Value::str(got.to_string()));
        attrs.insert("message".to_string(), Value::str(msg.clone()));
        Self::typed("X::TypeCheck::Binding::Parameter", attrs)
    }

    /// Serialize this error as a JSON document of the form
    /// `{"<ClassName>":{<attr>:<value>, ...}}`.
    ///
    /// This is the format used by the `RAKU_EXCEPTIONS_HANDLER=JSON`
    /// environment-variable exception handler. The object always contains a
    /// `message` key (null when the exception has no message attribute).
    pub fn to_json_exception(&self) -> String {
        let (class_name, attrs): (String, HashMap<String, Value>) = match &self.exception {
            Some(boxed) => match boxed.as_ref() {
                Value::Instance {
                    class_name,
                    attributes,
                    ..
                } => (
                    class_name.resolve(),
                    attributes
                        .as_map()
                        .iter()
                        .map(|(k, v)| (k.clone(), v.clone()))
                        .collect(),
                ),
                other => ("X::AdHoc".to_string(), {
                    let mut m = HashMap::new();
                    m.insert("message".to_string(), Value::str(other.to_string_value()));
                    m
                }),
            },
            None => ("X::AdHoc".to_string(), {
                let mut m = HashMap::new();
                m.insert("message".to_string(), Value::str(self.message.clone()));
                m
            }),
        };

        // Emit attributes in a stable order: `message` first, then the rest
        // sorted by name. Always include `message` (null if absent).
        let mut keys: Vec<&String> = attrs.keys().collect();
        keys.sort();
        let mut parts: Vec<String> = Vec::new();
        parts.push(format!(
            "\"message\":{}",
            match attrs.get("message") {
                Some(v) => json_value(v),
                None => "null".to_string(),
            }
        ));
        for k in keys {
            if k == "message" {
                continue;
            }
            parts.push(format!("{}:{}", json_string(k), json_value(&attrs[k])));
        }

        format!("{{{}:{{{}}}}}", json_string(&class_name), parts.join(","))
    }
}

/// Encode a Rust string as a JSON string literal (with surrounding quotes).
fn json_string(s: &str) -> String {
    let mut out = String::with_capacity(s.len() + 2);
    out.push('"');
    for c in s.chars() {
        match c {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            c if (c as u32) < 0x20 => out.push_str(&format!("\\u{:04x}", c as u32)),
            c => out.push(c),
        }
    }
    out.push('"');
    out
}

/// Encode a runtime Value as a JSON value for the exception handler.
fn json_value(v: &Value) -> String {
    match v {
        Value::Nil => "null".to_string(),
        Value::Bool(b) => b.to_string(),
        Value::Int(n) => n.to_string(),
        Value::Num(f) => {
            if f.is_finite() {
                f.to_string()
            } else {
                "null".to_string()
            }
        }
        Value::Str(s) => json_string(s),
        Value::Array(items, _) => {
            let elems: Vec<String> = items.iter().map(json_value).collect();
            format!("[{}]", elems.join(","))
        }
        Value::Hash(map) => {
            let mut keys: Vec<&String> = map.keys().collect();
            keys.sort();
            let pairs: Vec<String> = keys
                .iter()
                .map(|k| format!("{}:{}", json_string(k), json_value(&map[*k])))
                .collect();
            format!("{{{}}}", pairs.join(","))
        }
        other => json_string(&other.to_string_value()),
    }
}
