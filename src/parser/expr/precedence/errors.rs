use super::*;

pub(crate) fn syntax_exception(class_name: &str, message: impl Into<String>) -> PError {
    let message = message.into();
    let mut attrs = HashMap::new();
    attrs.insert("message".to_string(), Value::str(message.clone()));
    let exception = Value::make_instance(Symbol::intern(class_name), attrs);
    PError::fatal_with_exception(message, Box::new(exception))
}

pub(crate) fn worry_precedence_range(action: &str) -> PError {
    let message = format!(
        "To {} a range, parenthesize the whole range.\n(Or parenthesize the whole endpoint expression, if you meant that.)",
        action
    );
    let mut attrs = HashMap::new();
    attrs.insert("message".to_string(), Value::str(message.clone()));
    attrs.insert("action".to_string(), Value::str(action.to_string()));
    let exception = Value::make_instance(Symbol::intern("X::Worry::Precedence::Range"), attrs);
    PError::fatal_with_exception(message, Box::new(exception))
}

/// Check if the left expression of a range operator is a bare prefix `|` or `~`
/// (not parenthesized), which creates a precedence ambiguity warning.
/// We check the original input text rather than the AST because parentheses
/// are transparent in the AST (`(|4)` and `|4` produce the same tree).
pub(crate) fn check_range_precedence_worry(input: &str) -> Result<(), PError> {
    let trimmed = input.trim_start();
    if trimmed.starts_with('|') && !trimmed.starts_with("|(") {
        return Err(worry_precedence_range("apply a Slip flattener to"));
    }
    if trimmed.starts_with('~') && !trimmed.starts_with("~(") {
        return Err(worry_precedence_range("stringify"));
    }
    Ok(())
}

pub(crate) fn non_list_associative_error(lhs: &str, rhs: &str) -> PError {
    syntax_exception(
        "X::Syntax::NonListAssociative",
        format!(
            "Only identical operators may be list associative; since '{}' and '{}' differ, they are non-associative and you need to clarify with parentheses",
            lhs, rhs
        ),
    )
}

pub(crate) fn non_associative_error(op_name: &str) -> PError {
    syntax_exception(
        "X::Syntax::NonAssociative",
        format!("Non-associative operator '{}' cannot be chained", op_name),
    )
}

/// Non-associative chain of two named operators (e.g. `1 <=> 2 <=> 3`).
/// Carries `.left` / `.right` like rakudo's `X::Syntax::NonAssociative`.
pub(crate) fn non_associative_pair_error(left: &str, right: &str) -> PError {
    let message = format!(
        "Operators '{}' and '{}' are non-associative and require parentheses",
        left, right
    );
    let mut attrs = HashMap::new();
    attrs.insert("message".to_string(), Value::str(message.clone()));
    attrs.insert("left".to_string(), Value::str(left.to_string()));
    attrs.insert("right".to_string(), Value::str(right.to_string()));
    let exception = Value::make_instance(Symbol::intern("X::Syntax::NonAssociative"), attrs);
    PError::fatal_with_exception(message, Box::new(exception))
}

pub(crate) fn conditional_precedence_too_loose_error() -> PError {
    syntax_exception(
        "X::Syntax::ConditionalOperator::PrecedenceTooLoose",
        "Assignment operators inside ?? !! are too loose; parenthesize them",
    )
}
