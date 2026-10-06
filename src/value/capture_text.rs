//! The text of a `Capture`: its `.gist` and its `.Str`.
//!
//! The two differ. `.gist` is the `.raku` call shape (`\(1, "x y", :a(3))`);
//! `.Str` is Rakudo's `(positional.map(~*), named.pairs.map(~*)).join(' ')`,
//! each positional argument's `.Str` and then each named `Pair`'s `.Str`
//! (`key<TAB>value`), joined with a space. Stringification (`~$c`, `"$c"`)
//! is the `.Str` form.

use crate::value::{Value, ValueMap, ValueView};

/// The named arguments by name, so the text does not depend on the map's
/// internal order.
// Cost: O(n log n), n = named arguments.
fn sorted_named(named: &ValueMap) -> Vec<(&String, &Value)> {
    let mut entries: Vec<_> = named.iter().collect();
    entries.sort_by(|a, b| a.0.cmp(b.0));
    entries
}

/// `Capture.raku`: `\(1, "x y", :a(3))`.
// Cost: O(p + n log n + t), p / n = positional / named arguments, t = rendered size.
pub(crate) fn capture_raku(positional: &[Value], named: &ValueMap) -> String {
    let mut parts = Vec::new();
    for v in positional {
        match v.view() {
            ValueView::Pair(k, val) => {
                parts.push(format!(
                    "{} => {}",
                    crate::value::raku_repr::raku_value(&Value::str(k.clone())),
                    crate::value::raku_repr::raku_value(val)
                ));
            }
            ValueView::ValuePair(k, val) => {
                parts.push(format!(
                    "{} => {}",
                    crate::value::raku_repr::raku_value(k),
                    crate::value::raku_repr::raku_value(val)
                ));
            }
            _ => parts.push(crate::value::raku_repr::raku_value(v)),
        }
    }
    let mut named_keys: Vec<&String> = named.keys().collect();
    named_keys.sort();
    for k in named_keys {
        let v = &named[k];
        if let ValueView::Bool(true) = v.view() {
            parts.push(format!(":{}", k));
        } else if let ValueView::Bool(false) = v.view() {
            parts.push(format!(":!{}", k));
        } else {
            parts.push(format!(
                ":{}({})",
                k,
                crate::value::raku_repr::raku_value(v)
            ));
        }
    }
    format!("\\({})", parts.join(", "))
}

/// `Capture.gist` is exactly its `.raku` form.
// Cost: O(p + n log n + t), p / n = positional / named arguments, t = rendered size.
pub(crate) fn capture_gist(positional: &[Value], named: &ValueMap) -> String {
    capture_raku(positional, named)
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
