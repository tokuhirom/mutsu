//! Display names of types renamed with `.^set_name`.
//!
//! `T.^set_name('X')` renames the type itself in rakudo: every type object of
//! it, wherever it is reached from (`$obj.WHAT`, an element of `.^mro`),
//! answers `X` to `.^name` and gists as `(X)`. mutsu keeps the type's
//! registry identity (its key) and records the new name in the type's
//! metadata, which `.^name` reads; but a type object's `.gist` / `.Str` is
//! rendered by the display layer ([`super::display`]), which has no
//! interpreter context. This table maps each renamed key to its current name
//! so that rendering sees the rename too. It is process-global for the same
//! reason as [`super::enum_display`]: a rename in rakudo is a property of the
//! type, visible from every thread.
//!
//! Identity comparisons must keep reading the key; only a human-facing
//! rendering goes through here.

use std::collections::HashMap;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{OnceLock, RwLock};

/// Set once the first rename is recorded, so a program that renames no type
/// pays one load per rendered type object.
static ANY_ENTRY: AtomicBool = AtomicBool::new(false);

fn table() -> &'static RwLock<HashMap<String, String>> {
    static TABLE: OnceLock<RwLock<HashMap<String, String>>> = OnceLock::new();
    TABLE.get_or_init(|| RwLock::new(HashMap::new()))
}

/// Record that the type registered under `key` is now named `name`.
// Cost: O(1) amortized hash insert (plus the key/name copies).
pub(crate) fn note_type_rename(key: &str, name: &str) {
    if let Ok(mut map) = table().write() {
        map.insert(key.to_string(), name.to_string());
        ANY_ENTRY.store(true, Ordering::Release);
    }
}

/// The name the type registered under `key` was renamed to, or `None` when
/// it was never renamed.
// Cost: O(1) -- a flag load, then one hash probe once any entry exists.
pub(crate) fn type_rename_display(key: &str) -> Option<String> {
    if !ANY_ENTRY.load(Ordering::Acquire) {
        return None;
    }
    table().read().ok()?.get(key).cloned()
}
