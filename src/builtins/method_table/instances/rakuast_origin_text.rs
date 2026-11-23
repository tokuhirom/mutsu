//! `Origin.Str`: the source text a RakuAST node's origin covers (#18312).

use crate::value::{RuntimeError, Value, ValueView};

/// `Origin.Str`: the characters `from ..^ to` of the unit the origin indexes,
/// as rakudo's `RakuAST::Origin.Str` (`$y.foo` for the `ApplyPostfix` of
/// `try $y.foo`). An origin without a unit source answers the empty string.
// Cost: O(t), t = `to`, the end offset (the unit's text is walked by
// character up to it).
pub(super) fn origin_str(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let ValueView::Instance { attributes, .. } = target.view() else {
        return None;
    };
    let origin = attributes.as_map();
    let (ValueView::Int(from), ValueView::Int(to)) =
        (origin.get("from")?.view(), origin.get("to")?.view())
    else {
        return None;
    };
    let orig = origin.get("source").and_then(|source| match source.view() {
        ValueView::Instance { attributes, .. } => attributes
            .as_map()
            .get(crate::rakuast::unit_source::ORIG_ATTR)
            .map(Value::to_string_value),
        _ => None,
    });
    let Some(orig) = orig else {
        return Some(Ok(Value::str(String::new())));
    };
    let text: String = orig
        .chars()
        .skip(from.max(0) as usize)
        .take((to - from).max(0) as usize)
        .collect();
    Some(Ok(Value::str(text)))
}
