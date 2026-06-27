//! Structured error constructors for method dispatch failures
//! (X::Method::Private::*, X::Method::NotFound, X::Immutable, X::Multi::*).
use super::*;
use crate::symbol::Symbol;

/// Create a structured X::Method::Private::Permission error: a fully-qualified
/// private call `$o!Owner::meth` where `Owner` does not trust the calling
/// package. Carries `method`, `source-package` (the owner) and `calling-package`.
pub(super) fn make_private_permission_error(
    method_name: &str,
    class_name: &str,
    calling_package: &str,
) -> RuntimeError {
    let msg = format!(
        "Cannot call private method '{}' on package {} because it does not trust {}",
        method_name, class_name, calling_package
    );
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("method".to_string(), Value::str(method_name.to_string()));
    attrs.insert(
        "source-package".to_string(),
        Value::str(class_name.to_string()),
    );
    attrs.insert(
        "calling-package".to_string(),
        Value::str(calling_package.to_string()),
    );
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    let ex = Value::make_instance(Symbol::intern("X::Method::Private::Permission"), attrs);
    let mut err = RuntimeError::new(&msg);
    err.exception = Some(Box::new(ex));
    err
}

/// Create a structured X::Method::Private::Unqualified error: an unqualified
/// private call `$o!meth` on something other than `self`. Raku requires such
/// calls to name the package that defines the private method.
pub(super) fn make_private_unqualified_error(method_name: &str) -> RuntimeError {
    let msg = format!(
        "Calling private method '{}' must be fully qualified with the package containing that private method.",
        method_name
    );
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("method".to_string(), Value::str(method_name.to_string()));
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    let ex = Value::make_instance(Symbol::intern("X::Method::Private::Unqualified"), attrs);
    let mut err = RuntimeError::new(&msg);
    err.exception = Some(Box::new(ex));
    err
}

/// Create a structured X::Method::NotFound error.
pub(super) fn make_method_not_found_error(
    method_name: &str,
    type_name: &str,
    private: bool,
) -> RuntimeError {
    use super::did_you_mean::{known_methods_for_type, suggest_method};

    let mut msg = if private {
        format!(
            "No such private method '{}' for invocant of type '{}'",
            method_name, type_name
        )
    } else {
        format!(
            "No such method '{}' for invocant of type '{}'",
            method_name, type_name
        )
    };

    // Append "Did you mean ...?" suggestion if a close match exists
    if !private {
        let candidates = known_methods_for_type(type_name);
        if let Some(suggestion) = suggest_method(method_name, candidates) {
            msg.push_str(&format!("\nDid you mean '{}'?", suggestion));
        }
    }

    let mut attrs = std::collections::HashMap::new();
    attrs.insert("method".to_string(), Value::str(method_name.to_string()));
    attrs.insert("typename".to_string(), Value::str(type_name.to_string()));
    attrs.insert("private".to_string(), Value::Bool(private));
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    let ex = Value::make_instance(Symbol::intern("X::Method::NotFound"), attrs);
    let mut err = RuntimeError::new(&msg);
    err.exception = Some(Box::new(ex));
    err
}

/// Create a structured X::Immutable error.
pub(super) fn make_x_immutable_error(method_name: &str, typename: &str) -> RuntimeError {
    let msg = format!(
        "Cannot call '{}' on an immutable '{}'",
        method_name, typename
    );
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("method".to_string(), Value::str(method_name.to_string()));
    attrs.insert("typename".to_string(), Value::str(typename.to_string()));
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    let ex = Value::make_instance(Symbol::intern("X::Immutable"), attrs);
    let mut err = RuntimeError::new(&msg);
    err.exception = Some(Box::new(ex));
    err
}

/// Create a structured X::Multi::NoMatch error.
pub(super) fn make_multi_no_match_error(method_name: &str) -> RuntimeError {
    let msg = format!("No matching candidates for method: {}", method_name);
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    let ex = Value::make_instance(Symbol::intern("X::Multi::NoMatch"), attrs);
    let mut err = RuntimeError::new(&msg);
    err.exception = Some(Box::new(ex));
    err
}

/// Build an `X::Multi::Ambiguous` error naming the dispatch target and the
/// candidate signatures that all matched equally well.
pub(crate) fn make_multi_ambiguous_error(
    method_name: &str,
    invocant_type: &str,
    candidate_sigs: &[String],
) -> RuntimeError {
    let sig_lines = if candidate_sigs.is_empty() {
        String::new()
    } else {
        let mut s = String::new();
        for sig in candidate_sigs {
            s.push_str("\n  ");
            s.push_str(sig);
        }
        s
    };
    let msg = format!(
        "Ambiguous call to '{}({}: )'; these signatures all match:{}",
        method_name, invocant_type, sig_lines
    );
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    let ex = Value::make_instance(Symbol::intern("X::Multi::Ambiguous"), attrs);
    let mut err = RuntimeError::new(&msg);
    err.exception = Some(Box::new(ex));
    err
}

/// Build an `X::Multi::NoMatch` error with a Raku-style message that names the
/// dispatch target (invocant type + method name) and lists the candidate
/// signatures, e.g.:
///
/// ```text
/// Cannot resolve caller has_tie(WorkingTie:D: Array:D); none of these
/// signatures matches: ...
/// ```
pub(super) fn make_multi_no_match_error_detailed(
    method_name: &str,
    invocant_type: &str,
    candidate_sigs: &[String],
) -> RuntimeError {
    let sig_lines = if candidate_sigs.is_empty() {
        String::new()
    } else {
        let mut s = String::new();
        for sig in candidate_sigs {
            s.push_str("\n    ");
            s.push_str(sig);
        }
        s
    };
    let msg = format!(
        "Cannot resolve caller {}({}:D: ); none of these signatures matches:{}",
        method_name, invocant_type, sig_lines
    );
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    let ex = Value::make_instance(Symbol::intern("X::Multi::NoMatch"), attrs);
    let mut err = RuntimeError::new(&msg);
    err.exception = Some(Box::new(ex));
    err
}
