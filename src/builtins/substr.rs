//! Pure (engine-agnostic) `substr` fast-path slice.
//!
//! The simple `substr` case — a non-negative integer start with an optional
//! non-negative integer length — is a pure character slice. It was copied four
//! times across the native layer (`native_method_1arg`/`native_method_2arg` for
//! the `.substr` method, and the 2-/3-arg `substr` function in
//! `builtins::functions`). This collapses those copies into one shared
//! [`native_substr_slice`].
//!
//! Anything that needs interpreter state — a negative / `WhateverCode` start or
//! length (`substr(*-3, *-1)`), a `Range` argument (`substr(2..5)`), or an
//! out-of-range start (which must produce a `Failure` wrapping `X::OutOfRange`)
//! — stays in the interpreter's `dispatch_substr`. This helper returns `None`
//! for all of those so the caller defers, exactly as the four copies did.

use crate::value::{RuntimeError, Value};

/// Pure `substr` slice for a non-negative integer `start` and optional
/// non-negative integer `len`. Returns `None` (defer to the interpreter) when
/// `start`/`len` is negative or non-`Int`, or when `start` is past the end (the
/// interpreter then returns a `Failure`).
pub(crate) fn native_substr_slice(
    text: &str,
    start: &Value,
    len: Option<&Value>,
) -> Option<Result<Value, RuntimeError>> {
    let start = match start {
        Value::Int(i) if *i >= 0 => *i as usize,
        Value::Int(_) => return None, // negative: let the interpreter handle
        _ => return None,
    };
    let len = match len {
        Some(Value::Int(i)) if *i >= 0 => Some(*i as usize),
        Some(Value::Int(_)) => return None, // negative length
        Some(_) => return None,             // WhateverCode / Num / etc.
        None => None,
    };

    let chars: Vec<char> = text.chars().collect();
    if start > chars.len() {
        return None; // out-of-range: the interpreter returns a Failure
    }

    let slice: String = match len {
        Some(l) => {
            let end = (start + l).min(chars.len());
            chars[start..end].iter().collect()
        }
        None => chars[start..].iter().collect(),
    };
    Some(Ok(Value::str(slice)))
}
