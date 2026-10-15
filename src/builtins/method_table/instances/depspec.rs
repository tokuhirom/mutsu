//! `CompUnit::DependencySpecification`'s rows (ADR-11276 §9.115): what a
//! `use`, `need` or `$*REPO.resolve` asks for, its `short-name` and the
//! `version`/`auth`/`api` matchers.
//!
//! The instance carries the matchers a constructor was given as attributes; a
//! matcher left out is `True` (it matches anything), as in Rakudo.

use super::{Handler, MethodRow, RowFlags};
use crate::value::{RuntimeError, Value, ValueView};

type Answer = Option<Result<Value, RuntimeError>>;

macro_rules! row {
    ($name:literal, $handler:ident) => {
        MethodRow {
            owner: "CompUnit::DependencySpecification",
            name: $name,
            arity: 0,
            handler: Handler::Narrow($handler),
            flags: RowFlags::NONE,
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    row!("short-name", short_name),
    row!("version-matcher", version_matcher),
    row!("auth-matcher", auth_matcher),
    row!("api-matcher", api_matcher),
    row!("from", from),
    row!("Str", str_row),
    row!("raku", raku),
];

/// The attribute `key` of the instance, `None` when it was not given.
fn attr(target: &Value, key: &str) -> Option<Option<Value>> {
    let ValueView::Instance { attributes, .. } = target.view() else {
        return None;
    };
    Some(attributes.as_map().get(key).cloned())
}

/// A matcher: the one the constructor was given, or `True`.
fn matcher(target: &Value, key: &str) -> Answer {
    Some(Ok(attr(target, key)?.unwrap_or(Value::TRUE)))
}

/// `.short-name`.
// Cost: O(1).
fn short_name(target: &Value, _args: &[Value]) -> Answer {
    Some(Ok(attr(target, "short-name")?.unwrap_or(Value::NIL)))
}

/// `.version-matcher`.
// Cost: O(1).
fn version_matcher(target: &Value, _args: &[Value]) -> Answer {
    matcher(target, "version-matcher")
}

/// `.auth-matcher`.
// Cost: O(1).
fn auth_matcher(target: &Value, _args: &[Value]) -> Answer {
    matcher(target, "auth-matcher")
}

/// `.api-matcher`.
// Cost: O(1).
fn api_matcher(target: &Value, _args: &[Value]) -> Answer {
    matcher(target, "api-matcher")
}

/// `.from`: the language the module is written in, `Perl6` by default.
// Cost: O(1).
fn from(target: &Value, _args: &[Value]) -> Answer {
    Some(Ok(attr(target, "from")?.unwrap_or(Value::str_from("Perl6"))))
}

/// The given matchers in Rakudo's order, as `(adverb, attribute, text)`.
fn given_matchers(target: &Value) -> Option<Vec<(&'static str, &'static str, String)>> {
    let mut given = Vec::new();
    for (adverb, key) in [
        ("ver", "version-matcher"),
        ("auth", "auth-matcher"),
        ("api", "api-matcher"),
    ] {
        if let Some(value) = attr(target, key)? {
            given.push((adverb, key, value.to_string_value()));
        }
    }
    Some(given)
}

/// `.Str`: the short-name with an adverb per given matcher
/// (`Foo:ver<1.0+>:auth<zef:x>`).
// Cost: O(n), n = chars of the rendering.
fn str_row(target: &Value, _args: &[Value]) -> Answer {
    let mut text = attr(target, "short-name")?
        .map(|v| v.to_string_value())
        .unwrap_or_default();
    for (adverb, _, value) in given_matchers(target)? {
        text.push_str(&format!(":{adverb}<{value}>"));
    }
    Some(Ok(Value::str(text)))
}

/// `.raku`: `CompUnit::DependencySpecification.new(:short-name<Foo>, ...)`.
// Cost: O(n), n = chars of the rendering.
fn raku(target: &Value, _args: &[Value]) -> Answer {
    let short = attr(target, "short-name")?
        .map(|v| v.to_string_value())
        .unwrap_or_default();
    let mut parts = vec![format!(":short-name<{short}>")];
    for (_, key, value) in given_matchers(target)? {
        parts.push(format!(
            ":{key}({})",
            crate::value::raku_repr::raku_value(&Value::str(value))
        ));
    }
    Some(Ok(Value::str(format!(
        "CompUnit::DependencySpecification.new({})",
        parts.join(",")
    ))))
}
