#![allow(clippy::result_large_err)]
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value};
use std::collections::HashMap;

/// Implementation of `uniparse` / `parse-names`: takes a comma-separated list
/// of Unicode character names and returns the corresponding string.
/// Returns a Failure wrapping X::Str::InvalidCharName for unknown names.
pub(crate) fn uniparse_impl(input: &str) -> Result<Value, RuntimeError> {
    if input.is_empty() {
        return Ok(Value::str_from(""));
    }
    let mut result = String::new();
    for part in input.split(',') {
        let name = part.trim();
        if name.is_empty() {
            continue;
        }
        if let Some(ch) = crate::token_kind::lookup_unicode_char_by_name(name) {
            result.push(ch);
        } else if let Some(s) = crate::token_kind::lookup_emoji_sequence(name) {
            result.push_str(&s);
        } else {
            // Return a Failure wrapping X::Str::InvalidCharName
            let mut attrs = HashMap::new();
            attrs.insert("name".to_string(), Value::str_from(name));
            let msg = format!("Unrecognized character name [{}]", name);
            attrs.insert("message".to_string(), Value::str_from(&msg));
            let ex = Value::make_instance(Symbol::intern("X::Str::InvalidCharName"), attrs);
            let mut failure_attrs = HashMap::new();
            failure_attrs.insert("exception".to_string(), ex);
            return Ok(Value::make_instance(
                Symbol::intern("Failure"),
                failure_attrs,
            ));
        }
    }
    Ok(Value::str(result))
}
