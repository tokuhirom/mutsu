//! Committed syntax errors for brace-scoped package declarators.

use crate::parser::parse_result::PError;
use crate::{symbol::Symbol, value::Value};

/// All declarators describe this action as `package name`.
pub(crate) fn check_pseudo_package_in_decl(name: &str) -> Result<(), PError> {
    if crate::parser::primary::var::is_pseudo_package(name) {
        let action = "package name";
        let message = format!("Cannot use pseudo package {name} in {action}");
        let attrs = std::collections::HashMap::from([
            ("pseudo-package".to_string(), Value::str(name.to_string())),
            ("action".to_string(), Value::str(action.to_string())),
            ("message".to_string(), Value::str(message.clone())),
        ]);
        let exception =
            Value::make_instance(Symbol::intern("X::PseudoPackage::InDeclaration"), attrs);
        return Err(PError::fatal_with_exception(message, Box::new(exception)));
    }
    Ok(())
}

pub(super) fn require_block(rest: &str, what: &str) -> Result<(), PError> {
    if !rest.starts_with(';') {
        return Ok(());
    }
    let nested = !crate::parser::stmt::simple::snapshot_package_path().is_empty();
    let (class, message) = if nested {
        (
            "X::UnitScope::TooLate",
            format!("Too late for unit-scoped {what} definition;\nPlease use the block form."),
        )
    } else {
        (
            "X::UnitScope::MustHaveUnit",
            format!(
                "Semicolon form of '{what}' without 'unit' is illegal. You probably want to use 'unit {what}'"
            ),
        )
    };
    let mut error = PError::raw_with_what(message.clone(), Some(rest.len()), class, what);
    let exception = error
        .exception
        .take()
        .expect("raw_with_what carries its exception");
    Err(PError::fatal_with_exception_at(message, exception, rest))
}
