//! The text of a `Capture`: its `.gist` and its `.Str`.
//!
//! The two differ. `.gist` shows the call shape (`\(1, "x y", :a(3))`);
//! `.Str` is Rakudo's `(positional.map(~*), named.pairs.map(~*)).join(' ')`,
//! each positional argument's `.Str` and then each named `Pair`'s `.Str`
//! (`key<TAB>value`), joined with a space. Stringification (`~$c`, `"$c"`)
//! is the `.Str` form.

use crate::value::{Value, ValueMap, ValueView};

/// A named value as it reads inside a Capture gist: a `Str` is quoted, an
/// allomorph shows its constructor, anything else is its text.
// Cost: O(len of the rendered string).
fn gist_member(v: &Value) -> String {
    match v.view() {
        ValueView::Str(s) => format!("\"{}\"", *s),
        ValueView::Mixin(inner, mixins) => {
            if let Some(str_val) = mixins.get("Str") {
                let type_name = match inner.view() {
                    ValueView::Int(_) | ValueView::BigInt(_) => "IntStr",
                    ValueView::Num(_) => "NumStr",
                    _ => "Allomorph",
                };
                format!(
                    "{}.new({}, \"{}\")",
                    type_name,
                    inner.to_string_value(),
                    str_val.to_string_value()
                )
            } else {
                v.to_string_value()
            }
        }
        _ => v.to_string_value(),
    }
}

/// The named arguments by name, so the text does not depend on the map's
/// internal order.
// Cost: O(n log n), n = named arguments.
fn sorted_named(named: &ValueMap) -> Vec<(&String, &Value)> {
    let mut entries: Vec<_> = named.iter().collect();
    entries.sort_by(|a, b| a.0.cmp(b.0));
    entries
}

/// `Capture.gist`: `\(1, "x y", :a(3))`.
// Cost: O(p + n log n + t), p / n = positional / named arguments, t = rendered size.
pub(crate) fn capture_gist(positional: &[Value], named: &ValueMap) -> String {
    let mut parts = Vec::with_capacity(positional.len() + named.len());
    for v in positional {
        match v.view() {
            ValueView::Str(s) => parts.push(format!("\"{}\"", *s)),
            _ => parts.push(v.to_string_value()),
        }
    }
    for (k, v) in sorted_named(named) {
        match v.view() {
            ValueView::Bool(true) => parts.push(format!(":{k}(Bool::True)")),
            ValueView::Bool(false) => parts.push(format!(":{k}(Bool::False)")),
            _ => parts.push(format!(":{k}({})", gist_member(v))),
        }
    }
    format!("\\({})", parts.join(", "))
}

/// `Capture.Str`: the positional arguments' `.Str`, then each named pair as
/// `key<TAB>value`, joined with a space (`\(1, "x y", :a(3)).Str` is
/// `1 x y a<TAB>3`).
// Cost: O(p + n log n + t), p / n = positional / named arguments, t = rendered size.
pub(crate) fn capture_str(positional: &[Value], named: &ValueMap) -> String {
    let mut parts = Vec::with_capacity(positional.len() + named.len());
    for v in positional {
        parts.push(v.to_str_context());
    }
    for (k, v) in sorted_named(named) {
        parts.push(format!("{k}\t{}", v.to_str_context()));
    }
    parts.join(" ")
}
