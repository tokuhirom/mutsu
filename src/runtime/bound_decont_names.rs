//! Which variable names have ever been `:=`-bound to a container.
//!
//! A scalar bound to a container (`my $x := @a`) leaves a `__mutsu_bound_decont::<name>`
//! marker in the env (`update_bound_decont_marker`), and every plain store to a
//! scalar has to clear a stale one. The program-wide latch
//! (`bound_decont_active`) only says that SOME name ever got a marker -- any
//! `my $codes := text.NFD` sets it, so a loaded module such as `JSON::Fast` or
//! Cro set it for every program -- and after it the store asked the env chain
//! for its own name's marker: a memoized key plus a chain walk, about 700
//! instructions on every scalar store (#7817).
//!
//! This set answers the question of THIS name, as `atomic_names` does for
//! atomics. A name is recorded where the marker is written (the only place
//! that inserts one) and nothing else ever creates the key, so a clear bit
//! proves no marker for the name exists in any env. Two names may share a bit;
//! the loser merely takes the probe it took before. The set is process-global
//! (a thread's env is a copy of its parent's) and never cleared.

use super::*;
use std::sync::atomic::{AtomicU64, Ordering};

/// How many `u64` words [`BOUND_DECONT_NAMES`] spreads names over.
const WORDS: usize = 16;

static BOUND_DECONT_NAMES: [AtomicU64; WORDS] = [const { AtomicU64::new(0) }; WORDS];

/// The `(word, bit)` holding `name`: the top ten bits of a multiplicative hash
/// of its interned id.
// Cost: O(1).
#[inline(always)]
fn slot(name: Symbol) -> (usize, u64) {
    let index = ((u64::from(name.id())).wrapping_mul(0x9E37_79B9_7F4A_7C15) >> 54) as usize;
    (index >> 6, 1u64 << (index & 63))
}

impl Interpreter {
    /// Record that a `:=` bind marker is written under `name`.
    // Cost: O(1).
    pub(crate) fn note_bound_decont_name(name: Symbol) {
        let (word, bit) = slot(name);
        if BOUND_DECONT_NAMES[word].load(Ordering::Relaxed) & bit == 0 {
            BOUND_DECONT_NAMES[word].fetch_or(bit, Ordering::Relaxed);
        }
    }

    /// Whether a bind marker may exist under `name`: `false` proves none does.
    // Cost: O(1), one relaxed load.
    #[inline(always)]
    pub(crate) fn bound_decont_name_possible(name: Symbol) -> bool {
        let (word, bit) = slot(name);
        BOUND_DECONT_NAMES[word].load(Ordering::Relaxed) & bit != 0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A name is possible only after a marker was written under it.
    #[test]
    fn a_name_is_possible_only_once_a_marker_is_noted() {
        let marked = Symbol::intern("bound-decont-names-test-marked");
        Interpreter::note_bound_decont_name(marked);
        assert!(Interpreter::bound_decont_name_possible(marked));
        let clear = (0..256)
            .map(|n| Symbol::intern(&format!("bound-decont-names-test-clear-{n}")))
            .find(|name| !Interpreter::bound_decont_name_possible(*name));
        assert!(clear.is_some(), "some unrelated name must keep its bit clear");
    }
}
