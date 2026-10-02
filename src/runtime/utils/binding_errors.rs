//! `X::Parameter::RW` and the `X::TypeCheck::Binding::Parameter` builders
//! whose message names the offending value by its `.gist`/`.raku`; they need
//! the value renderers above `Value`, so they live here rather than with the
//! other `RuntimeError` constructors in `value/error_typed.rs` (issue #10779).

use crate::value::expected_type_object;
use crate::value::{RuntimeError, Value, ValueMap};

/// `X::Parameter::RW` — an `is rw` / `<->` parameter was handed a value
/// that has no container behind it, so there is nothing for the binding to
/// alias. Raku raises this at BIND time, before the body runs, both for an
/// ordinary routine call (`sub f($x is rw) {}; f(1)`) and for a `for` loop
/// over an immutable source (`for (1,2) -> $v is rw { }`, ADR-0045 rows
/// 19/30), with one wording; this constructor is that wording.
///
/// `symbol` is the parameter as written, sigil included (`$x`), and `.got`
/// is the offending value itself — raku renders it with `.gist`, not
/// `.raku`, so a `Str` shows unquoted (`but got 'a' (Str)`).
pub(crate) fn parameter_rw_not_container(symbol: &str, got: &Value) -> RuntimeError {
    let msg = format!(
        "Parameter '{}' expects a writable container (variable) as an argument,\n\
but got '{}' ({}) as a value without a container.",
        symbol,
        crate::runtime::utils::gist_value(got),
        crate::runtime::utils::got_type_name(got),
    );
    let mut attrs = ValueMap::default();
    attrs.insert("symbol".to_string(), Value::str(symbol.to_string()));
    attrs.insert("got".to_string(), got.clone());
    attrs.insert("message".to_string(), Value::str(msg));
    RuntimeError::typed("X::Parameter::RW", attrs)
}

/// Like `typecheck_binding_parameter`, but with raku's exact wording
/// ("expected T but got U (repr)", not "expected T, got U") and `.got`
/// carrying the actual offending value. Matches the hand-rolled format
/// used throughout `runtime/types/binding_signature.rs` for real routine
/// parameter binding; factored out here so a second call site (`for`-loop
/// parameter binding, which never goes through that binder) can share it.
///
/// `repr` is the `(repr)` tail (`""` for none). An object's repr is its
/// `.raku`, a method call, so `Interpreter::typecheck_binding_parameter_failure`
/// supplies it; `runtime::utils::value_short_repr` answers for every other value.
/// `hint` is rakudo's optional beginner hint (`X::TypeCheck.explain`),
/// appended to the explanation, e.g. "You have to pass an explicitly typed
/// array, ..." (`Interpreter::container_binding_hint`). As in rakudo, the
/// explanation (`expected ... but got ...` plus the hint) is passed
/// through `naive-word-wrapper`; the `Type check failed in binding to
/// parameter '...'; ` lead-in is not.
pub(crate) fn typecheck_binding_parameter_with_hint(
    param: &str,
    expected: &str,
    value: &Value,
    repr: &str,
    hint: Option<&str>,
) -> RuntimeError {
    let got_type = crate::runtime::utils::got_type_name(value);
    // Unlike several sibling constructors in this file, the class name is
    // NOT baked into this message: `RuntimeError::typed` copies it verbatim into
    // both the top-level uncaught display AND the exception's own
    // `.message`/`.Str`, and raku's own text for this exception has no
    // "X::...: " prefix on either.
    let mut explain = if repr.is_empty() {
        format!("expected {expected} but got {got_type}")
    } else {
        format!("expected {expected} but got {got_type} {repr}")
    };
    if let Some(hint) = hint {
        explain.push_str(". ");
        explain.push_str(hint);
    }
    let msg = format!(
        "Type check failed in binding to parameter '{param}'; {}",
        crate::word_wrap::naive_word_wrap(&explain, 72)
    );
    let mut attrs = ValueMap::default();
    attrs.insert("parameter".to_string(), Value::str(param.to_string()));
    attrs.insert("expected".to_string(), expected_type_object(expected));
    attrs.insert("got".to_string(), value.clone());
    attrs.insert("message".to_string(), Value::str(msg));
    RuntimeError::typed("X::TypeCheck::Binding::Parameter", attrs)
}

/// X::TypeCheck::Binding::Parameter for a `where` constraint. Raku exposes
/// the anonymous predicate as the expected value and includes the bound
/// value's gist in the one-line runtime message.
pub(crate) fn typecheck_binding_parameter_where(param: &str, value: &Value) -> RuntimeError {
    let got = crate::runtime::utils::got_type_name(value);
    let msg = format!(
        "Constraint type check failed in binding to parameter '{}'; expected anonymous constraint to be met but got {} ({})",
        param,
        got,
        crate::builtins::methods_0arg::raku_repr::raku_value(value),
    );
    let mut attrs = ValueMap::default();
    attrs.insert("parameter".to_string(), Value::str(param.to_string()));
    attrs.insert(
        "expected".to_string(),
        Value::str("anonymous constraint".to_string()),
    );
    attrs.insert("got".to_string(), value.clone());
    attrs.insert("message".to_string(), Value::str(msg));
    RuntimeError::typed("X::TypeCheck::Binding::Parameter", attrs)
}

/// X::TypeCheck::Binding::Parameter for a literal-value parameter
/// (`sub f("a") {}`, `-> 'about' {}`) whose argument does not equal the
/// literal. Raku reports `.expected`/`.got` as the literal/actual VALUES
/// themselves (not a type), the parameter name as `<anon>` (a literal
/// parameter is always positional and unnamed), and renders both sides
/// via `.raku` (`expected "b" but got "z"`, `expected 0 but got 1`).
pub(crate) fn typecheck_binding_parameter_literal(expected: &Value, got: &Value) -> RuntimeError {
    let expected_repr = crate::builtins::methods_0arg::raku_repr::raku_value(expected);
    let got_repr = crate::builtins::methods_0arg::raku_repr::raku_value(got);
    let msg = format!(
        "Constraint type check failed in binding to parameter '<anon>'; expected {} but got {}",
        expected_repr, got_repr
    );
    let mut attrs = ValueMap::default();
    attrs.insert("parameter".to_string(), Value::str("<anon>".to_string()));
    attrs.insert("expected".to_string(), expected.clone());
    attrs.insert("got".to_string(), got.clone());
    attrs.insert("message".to_string(), Value::str(msg));
    RuntimeError::typed("X::TypeCheck::Binding::Parameter", attrs)
}
