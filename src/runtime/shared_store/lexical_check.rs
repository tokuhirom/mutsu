//! `MUTSU_LANE_LEXICAL_CHECK`: the ADR-16595 invariant oracle (#17837).
//!
//! Since ADR-16595 slice 5 no spawn publishes a user lexical into the
//! lineage store, and no code path should read or write one there by name: a
//! thread reaches a lexical through its captured cell. With the variable set,
//! every store access under a plain user-lexical key is reported once per
//! `(op, key)` on stderr:
//!
//! ```text
//! lane-lexical-check: op=set key=@a
//! ```
//!
//! Unset, every hook is one cached-flag test.

use std::collections::HashSet;
use std::sync::{Mutex, OnceLock};

/// Whether the oracle is on.
// Cost: O(1) after the first call.
pub(crate) fn enabled() -> bool {
    static ON: OnceLock<bool> = OnceLock::new();
    *ON.get_or_init(|| {
        std::env::var("MUTSU_LANE_LEXICAL_CHECK").is_ok_and(|v| !v.is_empty() && v != "0")
    })
}

/// Whether `key` names a plain user lexical (`x`, `@a`, `%h`, `&f`) rather
/// than a dynamic, an attribute, a twigil'd or special variable, a qualified
/// name or a runtime-internal `__mutsu_*` key.
// Cost: O(|key|).
pub(crate) fn is_user_lexical_key(key: &str) -> bool {
    let bare = key.strip_prefix(['@', '%', '&', '$']).unwrap_or(key);
    let mut chars = bare.chars();
    let Some(first) = chars.next() else {
        return false;
    };
    (first.is_alphabetic() || (first == '_' && !bare.starts_with("__") && bare.len() > 1))
        && !bare.contains("::")
        && !bare.contains(['*', '!', '?', '.', '^', '='])
        && bare != "self"
}

/// Report one access. Only called when [`enabled`].
// Cost: O(|key|) plus one hash insert per distinct shape.
pub(crate) fn note(key: &str, op: &'static str) {
    if !is_user_lexical_key(key) {
        return;
    }
    static SEEN: OnceLock<Mutex<HashSet<String>>> = OnceLock::new();
    let shape = format!("{op}:{key}");
    if SEEN
        .get_or_init(|| Mutex::new(HashSet::new()))
        .lock()
        .unwrap_or_else(|e| e.into_inner())
        .insert(shape)
    {
        eprintln!("lane-lexical-check: op={op} key={key}");
    }
}
