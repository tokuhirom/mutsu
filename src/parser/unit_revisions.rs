//! The language revision each loaded compilation unit was written for
//! (ADR-17287).
//!
//! `use v6.e` is a property of the unit that declares it, but the parser's
//! `CURRENT_LANGUAGE_VERSION` is a thread-local that holds whatever unit was
//! parsed last -- the main script. A routine from a module that declared a
//! different revision therefore ran under the caller's. The routine stack
//! (`runtime::routine_stack`) swaps the thread-local to the revision of the
//! file a frame's routine was declared in while the frame is live, using this
//! table. The table matters only once two loaded units disagree
//! ([`revisions_mixed`]); a program whose units all share one revision never
//! pays for it.

use crate::symbol::Symbol;
use std::collections::HashMap;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{LazyLock, RwLock};

static UNIT_REVISIONS: LazyLock<RwLock<HashMap<Symbol, Symbol>>> =
    LazyLock::new(|| RwLock::new(HashMap::new()));
static MIXED: AtomicBool = AtomicBool::new(false);
std::thread_local! {
    /// Whether a unit loaded on this thread declared 6.e. Per thread, like the
    /// parser's own current revision: a worker thread's routines get theirs
    /// from the frame swap, and a process-wide flag would leak between the
    /// interpreters one test process builds.
    static ANY_6E: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
}

/// Whether the 6.e methods are visible: the running unit is 6.e, or any unit
/// of the process declared 6.e. Rakudo adds the 6.e methods to the core types
/// when the first 6.e compunit loads, so a 6.d unit of the same process sees
/// them too (`.can('snip')`, `.snip`, `Str.nomark`).
// Cost: O(1), two thread-local reads.
#[inline]
pub(crate) fn revision_e_methods_visible() -> bool {
    ANY_6E.get() || crate::parser::current_language_version_starts_with("6.e")
}

/// Record that the unit in `file` declares language `version`.
// Cost: O(u), u = recorded units (the disagreement check), once per unit load.
pub(crate) fn note_unit_language_revision(file: Symbol, version: &str) {
    if version.starts_with("6.e") {
        ANY_6E.set(true);
    }
    let version = Symbol::intern(version);
    let mut units = UNIT_REVISIONS
        .write()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    units.insert(file, version);
    if units.values().any(|v| *v != version) {
        MIXED.store(true, Ordering::Relaxed);
    }
}

/// Whether two recorded units declare different revisions.
// Cost: O(1).
#[inline]
pub(crate) fn revisions_mixed() -> bool {
    MIXED.load(Ordering::Relaxed)
}

/// The revision recorded for the unit in `file`.
// Cost: O(1) expected; one read-locked probe.
pub(crate) fn unit_language_revision(file: Symbol) -> Option<Symbol> {
    UNIT_REVISIONS
        .read()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .get(&file)
        .copied()
}
