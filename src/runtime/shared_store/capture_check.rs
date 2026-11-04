//! `MUTSU_SPAWN_CAPTURE_CHECK`: an oracle for ADR-12204 slice S1.
//!
//! ADR-12204 proposes that a spawned block captures the `@`/`%` lexicals it
//! references as shared cells and gives them no name-keyed lane entry. That is
//! sound only for names the capture analysis sees. This oracle lists the lane
//! writes it does not see: every spawn from a known block records the
//! aggregate names the block (transitively) references, and a later write to
//! an aggregate's lane entry (`@a`, `%h`, or the `__mutsu_atomic_arr::@a` /
//! `__mutsu_atomic_hash::%h` element lanes) for a name no spawn captured is
//! reported once on stderr. Reads are not reported: the lane is also read by
//! the sync passes over every seeded entry, which says nothing about who uses
//! the name.
//!
//! ```text
//! spawn-capture-check: uncaptured-lane-write name=@a op=set
//! ```
//!
//! A reported name is an opaque shape (a registered class/role method body, an
//! rw-argument sink, a string `EVAL`, a block-less spawn such as a supply
//! driver or `.then`) the analysis must learn before the cell capture may cover
//! it. Nothing here changes behaviour; with the variable unset every hook is a
//! single cached-flag test.

use std::cell::Cell;
use std::collections::HashSet;
use std::sync::{Mutex, OnceLock};

thread_local! {
    /// Depth of `clone_for_thread` seeding on this thread: the lane writes it
    /// makes publish the spawner's lexicals, not a use of the name.
    static SEEDING: Cell<u32> = const { Cell::new(0) };
}

/// Marks the seeding half of a spawn on this thread until dropped.
pub(crate) struct SeedingGuard(bool);

impl SeedingGuard {
    // Cost: O(1).
    pub(crate) fn new() -> Self {
        let on = enabled();
        if on {
            SEEDING.with(|d| d.set(d.get() + 1));
        }
        SeedingGuard(on)
    }
}

impl Drop for SeedingGuard {
    fn drop(&mut self) {
        if self.0 {
            SEEDING.with(|d| d.set(d.get() - 1));
        }
    }
}


/// What `MUTSU_SPAWN_CAPTURE_CHECK` asks for: `0` nothing (unset, `0` or empty),
/// `1` the uncaptured lane writes, `2` (`all`) also every spawn's captured set.
fn level() -> u8 {
    static LEVEL: OnceLock<u8> = OnceLock::new();
    *LEVEL.get_or_init(|| match std::env::var("MUTSU_SPAWN_CAPTURE_CHECK").as_deref() {
        Err(_) | Ok("0") | Ok("") => 0,
        Ok("all") => 2,
        Ok(_) => 1,
    })
}

/// Whether the oracle is on.
// Cost: O(1) after the first call.
pub(crate) fn enabled() -> bool {
    level() != 0
}

/// The aggregate names some spawn's block captured (process-wide, by bare
/// name with its sigil, as the lane keys them).
fn captured() -> &'static Mutex<HashSet<String>> {
    static SET: OnceLock<Mutex<HashSet<String>>> = OnceLock::new();
    SET.get_or_init(|| Mutex::new(HashSet::new()))
}

/// The aggregate names some spawn captured by a shared cell (S2): they must
/// have no lane entry, so a later lane write to one is a bug.
fn cell_captured() -> &'static Mutex<HashSet<String>> {
    static SET: OnceLock<Mutex<HashSet<String>>> = OnceLock::new();
    SET.get_or_init(|| Mutex::new(HashSet::new()))
}

/// Record the `@`/`%` names a spawn captured by cell instead of the lane.
// Cost: O(n), n = names.
pub(crate) fn record_cell_captures<'a>(names: impl IntoIterator<Item = &'a String>) {
    let mut set = cell_captured().lock().unwrap_or_else(|e| e.into_inner());
    for name in names {
        if name.starts_with(['@', '%']) {
            set.insert(name.clone());
            if level() == 2 {
                eprintln!("spawn-capture-check: cell-captured {name}");
            }
        }
    }
}

/// What has been reported already (`op:name`), so each shape prints once.
fn reported() -> &'static Mutex<HashSet<String>> {
    static SET: OnceLock<Mutex<HashSet<String>>> = OnceLock::new();
    SET.get_or_init(|| Mutex::new(HashSet::new()))
}

/// Record the `@`/`%` names a spawned block references.
// Cost: O(n), n = names.
pub(crate) fn record_spawn_captures<'a>(names: impl IntoIterator<Item = &'a String>) {
    let mut set = captured().lock().unwrap_or_else(|e| e.into_inner());
    let mut this_spawn: Vec<&str> = Vec::new();
    for name in names {
        if name.starts_with(['@', '%']) {
            set.insert(name.clone());
            this_spawn.push(name);
        }
    }
    if level() == 2 {
        this_spawn.sort_unstable();
        eprintln!("spawn-capture-check: spawn captures [{}]", this_spawn.join(", "));
    }
}

/// The aggregate name a lane key is about: the key itself for `@a`/`%h`, the
/// base name for an atomic element lane, `None` for any other key.
fn aggregate_name(key: &str) -> Option<&str> {
    let name = key
        .strip_prefix("__mutsu_atomic_arr::")
        .or_else(|| key.strip_prefix("__mutsu_atomic_hash::"))
        .unwrap_or(key);
    name.starts_with(['@', '%']).then_some(name)
}

/// Report a lane write (`op` is `set` or `mut`) of an aggregate no spawn
/// captured. Only called when [`enabled`].
// Cost: O(1) expected (two hash lookups; one print per distinct shape).
pub(crate) fn note_lane_access(key: &str, op: &'static str) {
    let Some(name) = aggregate_name(key) else {
        return;
    };
    if SEEDING.with(|d| d.get()) > 0 {
        return;
    }
    if cell_captured()
        .lock()
        .unwrap_or_else(|e| e.into_inner())
        .contains(name)
        && reported()
            .lock()
            .unwrap_or_else(|e| e.into_inner())
            .insert(format!("cell:{op}:{name}"))
    {
        eprintln!("spawn-capture-check: cell-captured-lane-write name={name} op={op}");
    }
    if captured()
        .lock()
        .unwrap_or_else(|e| e.into_inner())
        .contains(name)
    {
        return;
    }
    let shape = format!("{op}:{name}");
    if reported()
        .lock()
        .unwrap_or_else(|e| e.into_inner())
        .insert(shape)
    {
        eprintln!("spawn-capture-check: uncaptured-lane-write name={name} op={op}");
    }
}
