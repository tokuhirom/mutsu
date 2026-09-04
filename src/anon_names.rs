//! Where the anonymous-declaration registry names come from.
//!
//! An anonymous declaration has no source name, so the parser mints one:
//! `__ANON_CLASS_N__`, `__ANON_ROLE_N__`, `__ANON_SUBSET_N__`, and the
//! `decl_id` ADR-0047 D1 mangles into every lexical class's registry key. All
//! four are drawn from **process-global** counters, and that is correct for
//! execution: two declaration sites in two different compilation units must
//! never collide in a process-global registry, and `next_anon_role_name` is
//! deliberately shared with the runtime's `but`-mixin path so a mixed-in
//! anonymous role and a parsed `role { }` cannot render the same `<anon|N>`.
//!
//! It is *not* correct for a resident analysis server. Interned strings are
//! leaked for the process lifetime by design (see [`crate::symbol`]), so every
//! re-parse of a document containing one anonymous declaration permanently
//! leaks one more name — measured at 1.00 interned symbol and ~0.5 KiB per
//! parse over 8000 re-parses (ADR-0065's S0 probe,
//! `tests/long_lived_parse.rs`). It was the only unbounded component that probe
//! found.
//!
//! An analysis-only parse never registers a type: nothing executes, no
//! `ClassDef` reaches the registry, and the names exist only to be printed back
//! as `documentSymbol` entries. So *for that mode only* the uniqueness
//! requirement drops from process-global to compilation-unit-local, and
//! [`with_unit_local_names`] switches the counters accordingly.
//!
//! Note what this is NOT: resetting the global counters per parse. That would
//! let two declaration sites in two different units collide in the shared
//! registry — the failure the counters exist to prevent. The unit-local mode is
//! safe precisely because nothing it names is ever registered.

use std::cell::RefCell;
use std::sync::atomic::{AtomicU64, Ordering};

/// The counters a single analysis parse draws from. One per *compilation
/// unit*, not per declaration site, so a nested sub-parse of the same document
/// keeps numbering upward rather than restarting.
struct UnitLocalCounters {
    class: u64,
    role: u64,
    subset: u64,
    decl_id: u64,
}

impl UnitLocalCounters {
    /// `decl_id` starts at 1 because 0 is its "no stable site" sentinel (see
    /// `crate::ast::next_class_decl_id`); the rest start where their globals do.
    fn new() -> Self {
        Self {
            class: 0,
            role: 0,
            subset: 0,
            decl_id: 1,
        }
    }
}

/// Which counter a mint is drawing from.
#[derive(Clone, Copy)]
pub(crate) enum AnonKind {
    Class,
    Role,
    Subset,
    DeclId,
}

thread_local! {
    /// `Some` for the duration of an analysis-only parse on this thread. The
    /// parser's own state is thread-local too, so a server thread analysing one
    /// document cannot see another thread's counters.
    static UNIT_LOCAL: RefCell<Option<UnitLocalCounters>> = const { RefCell::new(None) };
}

/// Restores the previous mode even if the body panics — the analysis entry
/// points catch panics *inside* this scope, but a future caller might not.
struct UnitLocalGuard {
    outermost: bool,
}

impl Drop for UnitLocalGuard {
    fn drop(&mut self) {
        if self.outermost {
            UNIT_LOCAL.with(|c| *c.borrow_mut() = None);
        }
    }
}

/// Run `f` with unit-local anonymous-name counters instead of the process-global
/// ones. For an ANALYSIS-only parse: see the module docs for why that is sound
/// there and nowhere else.
///
/// Re-entrant on purpose: a nested call keeps the OUTER unit's counters, so a
/// sub-parse within the same document cannot restart the numbering and give two
/// of its declarations the same name.
pub fn with_unit_local_names<R>(f: impl FnOnce() -> R) -> R {
    let outermost = UNIT_LOCAL.with(|c| {
        let mut slot = c.borrow_mut();
        if slot.is_none() {
            *slot = Some(UnitLocalCounters::new());
            true
        } else {
            false
        }
    });
    let _guard = UnitLocalGuard { outermost };
    f()
}

/// The next id for `kind`: unit-local while an analysis parse is running,
/// otherwise `global`.
pub(crate) fn next_id(kind: AnonKind, global: &AtomicU64) -> u64 {
    let unit_local = UNIT_LOCAL.with(|c| {
        c.borrow_mut().as_mut().map(|u| {
            let slot = match kind {
                AnonKind::Class => &mut u.class,
                AnonKind::Role => &mut u.role,
                AnonKind::Subset => &mut u.subset,
                AnonKind::DeclId => &mut u.decl_id,
            };
            let id = *slot;
            *slot += 1;
            id
        })
    });
    unit_local.unwrap_or_else(|| global.fetch_add(1, Ordering::Relaxed))
}
