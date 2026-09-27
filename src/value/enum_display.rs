//! Display names of package-scoped enum types.
//!
//! An enum declared inside a package (`module A { our enum pn <x y> }`) is a
//! type distinct from a same-named enum in another package, so its registry
//! identity is the package-qualified name `A::pn` -- the same key a nested
//! class gets (#9654). Rakudo nevertheless reports such an enum under the
//! name it was *declared* with: `A::pn.^name` is `pn`, `A::pn::x.raku` is
//! `pn::x` and the type object gists as `(pn)`. (A nested class keeps its
//! qualified name: `module A { class K { } }; A::K.^name` is `A::K`.)
//!
//! This table maps each such qualified identity to its declared name so the
//! display layer ([`super::user_facing_type_name`], which has no interpreter
//! context) can render it. It is process-global for the same reason the
//! NativeCall display table in `display.rs` is. Identity comparisons must keep
//! reading the qualified key; only a *human-facing* rendering goes through here.

use std::collections::HashMap;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{OnceLock, RwLock};

/// Set once the first entry is recorded, so a program that declares no
/// package-scoped enum pays one relaxed load per rendered type name.
static ANY_ENTRY: AtomicBool = AtomicBool::new(false);

fn table() -> &'static RwLock<HashMap<String, String>> {
    static TABLE: OnceLock<RwLock<HashMap<String, String>>> = OnceLock::new();
    TABLE.get_or_init(|| RwLock::new(HashMap::new()))
}

/// Record that the enum registered under `key` was declared as `declared`.
// Cost: O(1) amortized hash insert (plus the key/name copies).
pub(crate) fn note_enum_display_name(key: &str, declared: &str) {
    if key == declared {
        return;
    }
    if let Ok(mut map) = table().write() {
        map.insert(key.to_string(), declared.to_string());
        ANY_ENTRY.store(true, Ordering::Release);
    }
}

/// The declared name of the package-scoped enum registered under `key`, or
/// `None` when `key` is not such an enum (every other type displays as its key).
// Cost: O(1) -- a relaxed flag load, then one hash probe once any entry exists.
pub(crate) fn enum_display_name(key: &str) -> Option<String> {
    if !ANY_ENTRY.load(Ordering::Acquire) {
        return None;
    }
    table().read().ok()?.get(key).cloned()
}
