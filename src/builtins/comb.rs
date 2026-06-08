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

use crate::value::{RuntimeError, Value};
use unicode_segmentation::UnicodeSegmentation;

/// Apply a `comb` limit to a result vector: `Some(n)` keeps the first `n`
/// (and `n <= 0` yields empty); `None` keeps all.
fn apply_limit(result: Vec<Value>, limit: Option<i64>) -> Vec<Value> {
    match limit {
        Some(lim) if lim <= 0 => Vec::new(),
        Some(lim) => result.into_iter().take(lim as usize).collect(),
        None => result,
    }
}

/// Pure `.comb` split for the `Int` (chunk) and `Str` (fixed needle) matchers.
///
/// Returns `Some(items)` for those matchers (and `Some(empty)` when `limit <= 0`,
/// matching the interpreter's early-out). Returns `None` for any other matcher
/// (`Regex`, `Sub`, a bare value to be reinterpreted as a regex, or no matcher),
/// signalling the caller to fall back to the interpreter's regex path.
pub(crate) fn comb_pure(
    text: &str,
    matcher: Option<&Value>,
    limit: Option<i64>,
) -> Option<Vec<Value>> {
    // A non-positive limit yields empty regardless of matcher (mirrors the
    // interpreter's early return before matching).
    if matches!(limit, Some(lim) if lim <= 0) {
        return Some(Vec::new());
    }

    match matcher {
        Some(Value::Int(n)) => {
            let chunk_size = if *n <= 0 { 1usize } else { *n as usize };
            let graphemes: Vec<&str> = text.graphemes(true).collect();
            let result: Vec<Value> = graphemes
                .chunks(chunk_size)
                .map(|chunk| Value::str(chunk.concat()))
                .collect();
            Some(apply_limit(result, limit))
        }
        Some(Value::Str(needle)) => {
            if needle.is_empty() {
                let chars: Vec<Value> = text
                    .graphemes(true)
                    .map(|g| Value::str(g.to_string()))
                    .collect();
                return Some(apply_limit(chars, limit));
            }
            let mut result = Vec::new();
            let mut offset = 0usize;
            while offset <= text.len() {
                if let Some(lim) = limit
                    && result.len() >= lim as usize
                {
                    break;
                }
                let Some(pos) = text[offset..].find(needle.as_str()) else {
                    break;
                };
                let start = offset + pos;
                let end = start + needle.len();
                result.push(Value::str(text[start..end].to_string()));
                offset = end;
            }
            Some(result)
        }
        _ => None,
    }
}

/// Native `.comb(...)` for the pure matcher cases. Parses the positional
/// matcher + optional limit (the `:match` adverb only affects the regex path and
/// is ignored here, exactly as the interpreter ignores it for `Int`/`Str`).
/// Returns `None` to defer to the interpreter for `Regex`/`Sub`/bare matchers.
pub(crate) fn native_comb_method(
    target: &Value,
    args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    let text = target.to_string_value();

    // Separate positional args from the `:match` named pair (regex-only).
    let mut positional: Vec<&Value> = Vec::new();
    for arg in args {
        if let Value::Pair(key, _) = arg
            && key == "match"
        {
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

    comb_pure(&text, matcher, limit).map(|items| Ok(Value::Seq(std::sync::Arc::new(items))))
}
