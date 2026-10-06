//! The per-name gate in front of the name-keyed atomic-variable machinery.
//!
//! Atomic scalars have a legacy lane keyed by the variable's bare NAME
//! (`__mutsu_atomic_name::<name>` in the process-global shared store), and a
//! typed `atomicint` declaration is recorded under the name too
//! (`__mutsu_type::<name>`). Every read and every plain store of a variable has
//! to ask "is this one atomic?", and the answer is only ever "maybe" for a name
//! something registered atomic.
//!
//! The question used to be asked through a process-wide latch
//! (`atomic_var_seen_anywhere`): one `my atomicint $x` anywhere in the program
//! sent EVERY `GetLocal`, every typed declaration and every plain store, of
//! every variable, through the name-keyed cascade: a `format!` of the lane key,
//! an intern of it, two type-constraint probes, a shared-store read. In
//! `roast/S17-lowlevel/cas-int.t` that was most of the ~28k instructions per
//! `cas` loop iteration that a non-atomic loop of the same shape does not pay
//! ([#12120](https://github.com/tokuhirom/mutsu/issues/12120)).
//!
//! This set answers the same question of THIS name. It is the twin of the
//! type-constraint name set in `runtime_var_meta`, with one difference: the
//! callers here hold a `&str`, not a `Symbol`, so the bit is derived from a
//! hash of the name's bytes instead of its interned id. Reading it must not
//! intern — the intern was a large part of the cost it removes.
//!
//! Soundness is the same one-directional argument. A name is marked wherever
//! atomic storage can be registered for it (an `atomicint` constraint, or a
//! lane mapping created by `atomic_value_key_for_name`), and nothing else ever
//! writes either, so a clear bit proves neither exists. Two names may share a
//! bit; the loser merely takes the cascade it took before. The set is never
//! cleared.

use super::*;
use std::hash::Hasher;
use std::sync::atomic::{AtomicU64, Ordering};

/// How many `u64` words [`ATOMIC_NAMES`] spreads names over (a power of two).
const ATOMIC_NAME_WORDS: usize = 16;

/// Process-global, monotonic: which names have ever been registered as atomic,
/// as a 1024-bit set. Process-global for the reason the latch it refines is:
/// a worker thread's `cas` registers a name its parent's interpreter never saw.
static ATOMIC_NAMES: [AtomicU64; ATOMIC_NAME_WORDS] =
    [const { AtomicU64::new(0) }; ATOMIC_NAME_WORDS];

/// The `(word, bit)` of [`ATOMIC_NAMES`] holding `name`.
///
/// The `$` sigil is dropped, because the lane is keyed "by the canonical atomic
/// name, which may or may not carry the `$` sigil depending on how the op
/// spelled its argument" (`legacy_atomic_lane_owns`); `@`/`%`/`&` names stay
/// distinct from the scalar of the same spelling.
// Cost: O(m), m = name bytes.
#[inline(always)]
fn atomic_name_slot(name: &str) -> (usize, u64) {
    let canonical = name.strip_prefix('$').unwrap_or(name);
    let mut hasher = rustc_hash::FxHasher::default();
    hasher.write(canonical.as_bytes());
    // The top ten bits of the multiplicative hash are the well mixed ones.
    let index = (hasher.finish() >> 54) as usize;
    (index >> 6, 1u64 << (index & 63))
}

impl Interpreter {
    /// Record that atomic storage may exist under `name`: a typed `atomicint`
    /// declaration, or a legacy-lane mapping. Also latches the process-wide
    /// "any atomic" flag and the JIT's inline-read spoiler, as before.
    ///
    /// A name already recorded returns at once: the 4 threads of a `cas` loop
    /// all land here per call, and a write to the shared words each time would
    /// bounce their cache line for nothing.
    // Cost: O(m), m = name bytes; O(1) after the first call for a name.
    pub(crate) fn mark_atomic_var_seen(name: &str) {
        if Self::atomic_name_possible(name) {
            return;
        }
        let (word, bit) = atomic_name_slot(name);
        ATOMIC_NAMES[word].fetch_or(bit, Ordering::Relaxed);
        Self::note_atomic_var_seen_anywhere();
        crate::vm::vm_jit::note_atomic_local_read_spoiler();
    }

    /// Whether `name` could be an atomic variable. `false` proves no
    /// `atomicint` constraint and no legacy-lane mapping exists under it, so
    /// the read and reset paths skip the name-keyed cascade for it; `true`
    /// means "maybe", and the caller does what it did before this gate existed.
    ///
    /// The process-wide flag goes first: a program with no atomic at all pays
    /// one relaxed load, as it did.
    // Cost: O(1) when no atomic exists anywhere; O(m) otherwise, m = name bytes.
    #[inline(always)]
    pub(crate) fn atomic_name_possible(name: &str) -> bool {
        if !Self::atomic_var_seen_anywhere() {
            return false;
        }
        let (word, bit) = atomic_name_slot(name);
        ATOMIC_NAMES[word].load(Ordering::Relaxed) & bit != 0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A name is possible only after something registered it, spelled with or
    /// without the scalar sigil, and a name that was never registered stays
    /// clear, so an unrelated variable skips the cascade in a program that has
    /// atomics.
    #[test]
    fn a_name_is_possible_only_once_it_is_registered() {
        Interpreter::mark_atomic_var_seen("atomic-names-test-marked");
        assert!(Interpreter::atomic_var_seen_anywhere());
        assert!(Interpreter::atomic_name_possible(
            "atomic-names-test-marked"
        ));
        assert!(Interpreter::atomic_name_possible(
            "$atomic-names-test-marked"
        ));
        // 1024 bits and a handful of names in this process: pick a spelling
        // whose bit is clear rather than assume the hash never collides.
        let clear = (0..64)
            .map(|n| format!("atomic-names-test-clear-{n}"))
            .find(|name| !Interpreter::atomic_name_possible(name));
        assert!(
            clear.is_some(),
            "some unrelated name must keep its bit clear"
        );
    }
}
