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

/// `X::Syntax::CannotMeta` for `OP=` where `OP` is a chaining or structural
/// comparison operator (`6 >== 2`, `6 cmp= 2`, `6 ~~= 2`, `6 ..= 2`, ...).
/// rakudo's METAOP_ASSIGN refuses to compose an assignment out of a diffy
/// operator: `$x OP= $y` desugars to `$x = $x OP $y`, which only makes sense
/// when `OP` combines exactly two operands into one result. A chaining
/// comparison (`1 < 2 < 3`) and a non-associative structural one (`1 cmp 2`,
/// which cannot itself be chained) aren't that, so rakudo rejects the metaop
/// outright rather than silently picking a meaning. `dba` is rakudo's own
/// vocabulary for the operator category ("chaining" / "structural infix")
/// and appears verbatim in both the message and the `.dba` attribute;
/// `.meta`/`.operator`/`.reason` mirror rakudo's own `X::Syntax::CannotMeta`
/// attributes (verified against `raku -e '6 >== 2'`).
pub(crate) fn cannot_meta_assign_diffy_error(op: &str, dba: &str, remaining_len: usize) -> PError {
    let message =
        format!("Cannot make assignment out of {op} because {dba} operators are too diffy");
    let mut attrs = HashMap::new();
    attrs.insert("message".to_string(), Value::str(message.clone()));
    attrs.insert(
        "meta".to_string(),
        Value::str("make assignment out of".to_string()),
    );
    attrs.insert("operator".to_string(), Value::str(op.to_string()));
    attrs.insert("reason".to_string(), Value::str("too diffy".to_string()));
    attrs.insert("dba".to_string(), Value::str(dba.to_string()));
    let exception = Value::make_instance(Symbol::intern("X::Syntax::CannotMeta"), attrs);
    let mut err = PError::raw(
        format!("X::Syntax::CannotMeta: {message}"),
        Some(remaining_len),
    );
    err.exception = Some(Box::new(exception));
    err
}

/// `X::Syntax::ConditionalOperator::PrecedenceTooLoose`: something looser than
/// the conditional `?? !!` sits inside a branch and needs parentheses -- an
/// assignment operator (`$a ?? $a = 1 !! $a = 2`), the comma list separator
/// (`1 ?? 2,3 !! 4,5`), or a colonpair adverb (`1 ?? 3 :foo !! 2`). `op` is
/// rakudo's own spelling of the offending operator and appears verbatim in
/// both the message and the `.operator` attribute (verified against
/// `raku -e '...'` for each shape: the message is always exactly "Precedence
/// of {op} is too loose to use inside ?? !!; please parenthesize").
pub(crate) fn conditional_precedence_too_loose_error(op: &str) -> PError {
    let message =
        format!("Precedence of {op} is too loose to use inside ?? !!; please parenthesize");
    let mut attrs = HashMap::new();
    attrs.insert("message".to_string(), Value::str(message.clone()));
    attrs.insert("operator".to_string(), Value::str(op.to_string()));
    let exception = Value::make_instance(
        Symbol::intern("X::Syntax::ConditionalOperator::PrecedenceTooLoose"),
        attrs,
    );
    PError::fatal_with_exception(message, Box::new(exception))
}

/// `X::Syntax::ConditionalOperator::SecondPartInvalid`: the else-branch was
/// introduced with something other than `!!` -- rakudo recognizes `::` and a
/// bare `:` as typos for it (`1 ?? 3 :: 2`, `1 ?? 3 : 2`) and names the
/// offending spelling directly, both in the message and the `.second-part`
/// attribute.
pub(crate) fn conditional_second_part_invalid_error(second_part: &str) -> PError {
    let message = format!("Please use !! rather than {second_part}");
    let mut attrs = HashMap::new();
    attrs.insert("message".to_string(), Value::str(message.clone()));
    attrs.insert(
        "second-part".to_string(),
        Value::str(second_part.to_string()),
    );
    let exception = Value::make_instance(
        Symbol::intern("X::Syntax::ConditionalOperator::SecondPartInvalid"),
        attrs,
    );
    PError::fatal_with_exception(message, Box::new(exception))
}

/// `X::Syntax::ConditionalOperator::SecondPartGobbled`: the then-branch was a
/// bareword call parsed as a listop, which swallowed the `!!` as part of its
/// own argument list (`1 ?? rt123115 !! 3`, where `!! 3` parses as the
/// double-negation prefix operator applied to `3`, becoming `rt123115`'s sole
/// argument).
pub(crate) fn conditional_second_part_gobbled_error() -> PError {
    syntax_exception(
        "X::Syntax::ConditionalOperator::SecondPartGobbled",
        "Your !! was gobbled by the expression in the middle; please parenthesize",
    )
}
