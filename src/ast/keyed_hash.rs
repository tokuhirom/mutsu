//! A key-typed hash declaration, `my %h{Str}` / `my Int %h{Str}`.
//!
//! The parser folds the value and key types into one type-constraint string,
//! `"Int{Str}"`, and spells a missing value type as `Any`. The two spellings
//! `my %h{Str}` and `my Any %h{Str}` would then share one tree, so the parser
//! also records [`IMPLICIT_VALUE_TYPE`] on the declaration when it supplied
//! the `Any` itself. The compiler skips the marker like any `__` trait; the
//! RakuAST converter reads it to leave the declaration's `type` out.

/// The internal trait recording that a keyed hash's `Any` value type was not
/// written in the source.
pub(crate) const IMPLICIT_VALUE_TYPE: &str = "__implicit_value_type";

/// The `(value type, key type)` of a keyed hash's type-constraint string
/// (`"Int{Str}"` → `("Int", "Str")`), or `None` for any other type.
// Cost: O(k), k = length of the type string.
pub(crate) fn split(type_constraint: &str) -> Option<(&str, &str)> {
    let open = type_constraint.find('{')?;
    let key = type_constraint[open + 1..].strip_suffix('}')?;
    Some((&type_constraint[..open], key))
}

/// The type-constraint string for a keyed hash, the inverse of [`split`].
pub(crate) fn join(value_type: Option<&str>, key_type: &str) -> String {
    format!("{}{{{key_type}}}", value_type.unwrap_or("Any"))
}
