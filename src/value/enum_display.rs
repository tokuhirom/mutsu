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

/// The declared names themselves, so a lookup by a source spelling can ask
/// "is any package-scoped enum declared under this name?" without interning
/// anything (see `Interpreter::resolve_enum_type_key`).
fn declared_names() -> &'static RwLock<std::collections::HashSet<String>> {
    static NAMES: OnceLock<RwLock<std::collections::HashSet<String>>> = OnceLock::new();
    NAMES.get_or_init(|| RwLock::new(std::collections::HashSet::new()))
}

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
        if let Ok(mut names) = declared_names().write() {
            names.insert(declared.to_string());
        }
        ANY_ENTRY.store(true, Ordering::Release);
    }
}

/// Whether some package-scoped enum was declared under the short name `name`.
// Cost: O(1) -- a relaxed flag load, then one hash probe once any entry exists.
pub(crate) fn is_package_enum_declared_name(name: &str) -> bool {
    ANY_ENTRY.load(Ordering::Acquire)
        && declared_names()
            .read()
            .is_ok_and(|names| names.contains(name))
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
