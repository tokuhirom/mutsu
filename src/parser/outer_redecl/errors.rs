//! The typed compile-time errors the declaration-scope walk reports.

use super::ScopeDiagnostic;
use crate::value::{RuntimeError, RuntimeErrorCode, Value};

/// Turn a [`ScopeDiagnostic`] into the error rakudo raises for it.
pub(crate) fn scope_diagnostic_error(diag: ScopeDiagnostic) -> RuntimeError {
    match diag {
        ScopeDiagnostic::OuterRedeclaration(symbol, line) => {
            build_outer_redeclaration_error(&symbol, line)
        }
        ScopeDiagnostic::SelfInitializer(symbol, line) => {
            build_self_initializer_error(&symbol, line)
        }
        ScopeDiagnostic::HeredocHiddenName(symbol, line) => {
            build_heredoc_error("X::Syntax::Heredoc::HiddenName", &symbol, line)
        }
        ScopeDiagnostic::HeredocAmbiguousName(symbol, line) => {
            build_heredoc_error("X::Syntax::Heredoc::AmbiguousName", &symbol, line)
        }
    }
}

/// Build the compile-time error rakudo 2026.11 raises for a heredoc body name
/// that does not mean the same declaration where the heredoc starts and where
/// its body is written (`class` is `X::Syntax::Heredoc::HiddenName` or
/// `::AmbiguousName`).
fn build_heredoc_error(class: &str, symbol: &str, line: i64) -> RuntimeError {
    let mut attrs = crate::value::AttrMap::new();
    attrs.insert("symbol", Value::str(symbol.to_string()));
    let message = crate::value::exception_message::format_exception_message(class, &attrs)
        .unwrap_or_default();
    attrs.insert("message", Value::str(message.clone()));
    if line > 0 {
        attrs.insert("line", Value::int(line));
    }
    let mut err = RuntimeError::new(message);
    err.set_code(Some(RuntimeErrorCode::ParseGeneric));
    if line > 0 {
        err.set_line(Some(line as usize));
    }
    err.exception = Some(Box::new(Value::make_instance(
        crate::symbol::Symbol::intern(class),
        attrs,
    )));
    err
}

/// Build the compile-time error rakudo raises when a lexical is redeclared with
/// `my`/`state` after it has already been referenced as an outer symbol in the
/// same scope: `X::Redeclaration::Outer`.
fn build_outer_redeclaration_error(symbol: &str, line: i64) -> RuntimeError {
    let message = format!(
        "Lexical symbol '{sym}' is already bound to an outer symbol.  The implicit\n\
         outer binding must be rewritten as 'OUTER::<{sym}>' before you can\n\
         unambiguously declare a new '{sym}' in this scope.",
        sym = symbol
    );
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("message".to_string(), Value::str(message.clone()));
    attrs.insert("payload".to_string(), Value::str(message.clone()));
    attrs.insert("symbol-name".to_string(), Value::str(symbol.to_string()));
    attrs.insert("postfix".to_string(), Value::str(String::new()));
    attrs.insert("what".to_string(), Value::str("symbol".to_string()));
    if line > 0 {
        attrs.insert("line".to_string(), Value::int(line));
    }
    let mut err = RuntimeError::new(message);
    err.set_code(Some(RuntimeErrorCode::ParseGeneric));
    if line > 0 {
        err.set_line(Some(line as usize));
    }
    err.exception = Some(Box::new(Value::make_instance(
        crate::symbol::Symbol::intern("X::Redeclaration::Outer"),
        attrs,
    )));
    err
}

/// Build the compile-time error rakudo raises when a declaration's initializer
/// reads the variable being declared (`my $x = $x + 1`):
/// `X::Syntax::Variable::Initializer`.
fn build_self_initializer_error(symbol: &str, line: i64) -> RuntimeError {
    let message = format!("Cannot use variable {symbol} in declaration to initialize itself");
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("message".to_string(), Value::str(message.clone()));
    attrs.insert("payload".to_string(), Value::str(message.clone()));
    attrs.insert("name".to_string(), Value::str(symbol.to_string()));
    if line > 0 {
        attrs.insert("line".to_string(), Value::int(line));
    }
    let mut err = RuntimeError::new(message);
    err.set_code(Some(RuntimeErrorCode::ParseGeneric));
    if line > 0 {
        err.set_line(Some(line as usize));
    }
    err.exception = Some(Box::new(Value::make_instance(
        crate::symbol::Symbol::intern("X::Syntax::Variable::Initializer"),
        attrs,
    )));
    err
}
