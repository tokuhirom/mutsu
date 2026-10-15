//! The numeric value of the `Cool` instances with no shape of their own. The
//! methods this file once matched by name are rows: the transcendental math
//! methods (`method_table::scalars::math`), `succ`/`pred`/`sink`/`item`/`race`/
//! `hyper` (ADR-11276 §9.82), `Cool.Rat`/`Cool.FatRat` and `Any.tree` (§9.112).
use crate::value::{Value, ValueView};

/// Levenshtein edit distance between two strings (by Unicode scalar), used for
/// the numeric value of a `StrDistance`.
fn levenshtein(a: &str, b: &str) -> usize {
    let a: Vec<char> = a.chars().collect();
    let b: Vec<char> = b.chars().collect();
    let mut prev: Vec<usize> = (0..=b.len()).collect();
    let mut cur = vec![0usize; b.len() + 1];
    for (i, ca) in a.iter().enumerate() {
        cur[0] = i + 1;
        for (j, cb) in b.iter().enumerate() {
            let cost = if ca == cb { 0 } else { 1 };
            cur[j + 1] = (prev[j + 1] + 1).min(cur[j] + 1).min(prev[j] + cost);
        }
        std::mem::swap(&mut prev, &mut cur);
    }
    prev[b.len()]
}

/// The edit distance a `StrDistance` (the result of `tr///`) numifies to,
/// between its `before` and `after` strings.
// Cost: O(b * a), b and a = chars of the two strings.
fn str_distance(attributes: &crate::gc::Gc<crate::value::InstanceAttrs>) -> Option<usize> {
    let map = attributes.as_map();
    let text = |key: &str| map.get(key).map(|v| v.to_string_value()).unwrap_or_default();
    Some(levenshtein(&text("before"), &text("after")))
}

/// [`str_distance`] of a `StrDistance` instance, `None` for any other value:
/// what `Cool`'s numeric rows read it as (`numify`).
// Cost: as [`str_distance`].
pub(crate) fn str_distance_of(target: &Value) -> Option<usize> {
    match target.view() {
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "StrDistance" => str_distance(&attributes),
        _ => None,
    }
}

/// Real (numeric) value of a "Cool" builtin instance, read by `Mu`'s numeric
/// rows: an `IO::Path` numifies via its path string, a `Match` via its matched
/// substring, and a `StrDistance` via the edit distance between its
/// `before`/`after` strings.
// Cost: O(1) for a path or a match (its text parsed); as `str_distance` otherwise.
pub(crate) fn cool_instance_numeric(target: &Value) -> Option<f64> {
    // A grammar cursor is a Match (raku: `Grammar` IS a `Match` subclass) but
    // reports the grammar's own class name, so it is asked by shape, not name.
    if target.is_match_instance() {
        return target
            .match_str_value()
            .and_then(|v| v.to_string_value().trim().parse::<f64>().ok());
    }
    if let Some(distance) = str_distance_of(target) {
        return Some(distance as f64);
    }
    if target.dispatch_shape() == Some(crate::value::DispatchShape::IoPath) {
        return target.to_string_value().trim().parse::<f64>().ok();
    }
    None
}
