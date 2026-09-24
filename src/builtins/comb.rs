//! Pure (engine-agnostic) `.comb` matchers.
//!
//! `.comb` splits a string by a matcher. The *pure* matcher cases — an `Int`
//! chunk size and a fixed `Str` needle — need no interpreter state, so they live
//! here in the native layer as the single shared implementation, reachable by
//! both the VM (`native_method_1arg`/`native_method_2arg` -> `native_comb_method`)
//! and the interpreter (`dispatch_comb_with_args`, which calls [`comb_pure`]
//! instead of reimplementing the split). This follows the same layering as
//! `builtins::split`.
//!
//! The `Regex` matcher (and a bare matcher reinterpreted as a regex) genuinely
//! needs the interpreter-coupled regex engine (`regex_find_all`, code blocks,
//! `Match` objects), so those cases stay in `runtime/` — [`comb_pure`] returns
//! `None` for them, signalling the caller to defer.
//!
//! Every pure form returns the same lazy `Seq` Rakudo does: a
//! [`crate::value::StrIterSpec`] cursor that cuts the next piece only when a
//! consumer pulls it, so `.comb.head(3)` and `.comb(2, 3)` cost O(prefix).

use crate::value::{RuntimeError, StrIterMode, Value, ValueView, str_iter_seq};

/// Pure `.comb` for no matcher, the `Int` (chunk) matcher and the `Str`
/// (fixed needle) matcher, as a lazy `Seq` over `target`'s string.
///
/// A non-positive `limit` yields an empty Seq whatever the matcher. Returns
/// `None` for any other matcher (`Regex`, `Sub`, a bare value to be
/// reinterpreted as a regex), signalling the caller to fall back to the
/// interpreter's regex path.
// Cost: O(1); each pulled element then costs O(its bytes) (a needle pull also
// scans the bytes up to its match).
pub(crate) fn comb_pure(
    target: &Value,
    matcher: Option<&Value>,
    limit: Option<i64>,
) -> Option<Value> {
    // A non-positive limit yields empty regardless of matcher (mirrors the
    // interpreter's early return before matching).
    let limit = match limit {
        Some(lim) if lim <= 0 => return Some(Value::seq(Vec::new())),
        Some(lim) => Some(lim as usize),
        None => None,
    };
    let mode = match matcher.map(Value::view) {
        None => StrIterMode::Graphemes,
        Some(ValueView::Int(n)) if n <= 1 => StrIterMode::Graphemes,
        Some(ValueView::Int(n)) => StrIterMode::Chunks(n as usize),
        Some(ValueView::Str(needle)) if needle.is_empty() => StrIterMode::Graphemes,
        Some(ValueView::Str(needle)) => StrIterMode::Needle(std::sync::Arc::clone(&needle)),
        _ => return None,
    };
    Some(str_iter_seq(target, mode, limit))
}

/// Native `.comb(...)` for the pure matcher cases. Parses the positional
/// matcher + optional limit (the `:match` adverb only affects the regex path and
/// is ignored here, exactly as the interpreter ignores it for `Int`/`Str`).
/// Returns `None` to defer to the interpreter for `Regex`/`Sub`/bare matchers.
// Cost: O(1), see `comb_pure`.
pub(crate) fn native_comb_method(
    target: &Value,
    args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    // Separate positional args from named ones. `.comb` declares only `:match`
    // (regex-only, irrelevant to this pure path) and swallows every other named
    // through the implicit `*%_` every Raku method carries, so ALL named-flavour
    // `Pair`s are dropped -- leaving an unrecognized one in `positional` used to
    // read it as the `$limit`, numifying to 0 and making `"abc".comb(:nonsense)`
    // return an empty Seq. A *positional* `Pair` (the `ValuePair` flavour,
    // ADR-0021) is a real matcher argument and keeps its slot.
    let mut positional: Vec<&Value> = Vec::new();
    for arg in args {
        if matches!(arg.view(), ValueView::Pair(..)) {
            continue;
        }
        positional.push(arg);
    }

    let limit: Option<i64> = if positional.len() >= 2 {
        Some(positional[1].to_f64() as i64)
    } else {
        None
    };
    let matcher = positional.first().copied();

    comb_pure(target, matcher, limit).map(Ok)
}
