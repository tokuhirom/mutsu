use super::*;

/// Build an `X::Str::Numeric` error for a string that cannot be used as a
/// number, carrying rakudo's `source`, `pos` and `reason` attributes.
pub(crate) fn str_numeric_error(source: &str, pos: usize, reason: &str) -> RuntimeError {
    // Match Rakudo's `X::Str::Numeric.message`, which embeds the `⏏` position
    // marker via the source-indicator (e.g. `... in '5⏏ foo' (indicated by ⏏)`),
    // keeping this path consistent with `str_numeric_exception_attrs` (used by
    // the `.Int`/`.Num` coercions).
    let source_indicator = crate::runtime::str_numeric::build_source_indicator(source, pos);
    let msg = format!("Cannot convert string to number: {reason} {source_indicator}");
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("source".to_string(), Value::str(source.to_string()));
    attrs.insert("pos".to_string(), Value::int(pos as i64));
    attrs.insert("reason".to_string(), Value::str(reason.to_string()));
    attrs.insert("target-name".to_string(), Value::str("Numeric".to_string()));
    attrs.insert("source-indicator".to_string(), Value::str(source_indicator));
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    let ex = Value::make_instance(crate::symbol::Symbol::intern("X::Str::Numeric"), attrs);
    let mut err = RuntimeError::new(msg);
    err.exception = Some(Box::new(ex));
    err
}

/// Raise `X::Str::Numeric` if `value` is a `Str` (or allomorph wrapping one)
/// that cannot be parsed as a number. The arithmetic operators do NOT call
/// this — they evaluate to the lazy Failure of [`str_numeric_operand_failure`]
/// instead — and neither do the generic comparators (`cmp`, `before`/`after`,
/// `min`/`max`), which compare strings as strings.
pub(crate) fn check_str_numeric(value: &Value) -> Result<(), RuntimeError> {
    // Hot path: only a bare or Mixin-wrapped Str can fail; everything else
    // (Int/Num/Rat/...) returns immediately without cloning or parsing.
    let s = match value.view() {
        ValueView::Str(s) => s,
        ValueView::Mixin(inner, _) => match inner.view() {
            ValueView::Str(s) => s,
            _ => return Ok(()),
        },
        _ => return Ok(()),
    };
    if let Some((pos, reason)) = crate::runtime::str_numeric::str_numeric_failure(&s) {
        return Err(str_numeric_error(&s, pos, &reason));
    }
    Ok(())
}

/// The lazy `Failure` (wrapping X::Str::Numeric) an arithmetic operator
/// evaluates to when `value` is a `Str` (or allomorph wrapping one) that
/// cannot be parsed as a number; `None` for every other operand.
/// Cost: O(n), n = length of a Str operand; O(1) otherwise.
pub(crate) fn str_numeric_operand_failure(value: &Value) -> Option<Value> {
    let s = match value.view() {
        ValueView::Str(s) => s,
        ValueView::Mixin(inner, _) => match inner.view() {
            ValueView::Str(s) => s,
            _ => return None,
        },
        _ => return None,
    };
    crate::runtime::str_numeric::str_numeric_failure(&s)?;
    Some(crate::builtins::methods_0arg::str_numeric_failure(&s))
}
