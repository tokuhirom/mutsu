//! One env tier: a frame's overlay map, plus the indexes that are a pure
//! function of its **key set** and so may outlive any number of value writes.
//!
//! The map is private and every mutator lives here, which is the whole point:
//! a memo derived from the key set is only safe if no code path can add a key
//! behind its back, and making [`Tier::map`] unreachable is what turns that
//! from a review obligation into a compile-time one. A mutator that can only
//! REMOVE a key leaves the indexes alone: they are supersets by contract, and
//! their readers look each named key up rather than assume it is present.
//!
//! # Why a key-set-scoped memo at all
//!
//! Closure capture (`Interpreter::capture_closure_env` -> [`Env::filtered_flat_capture`])
//! walks every name visible from the creating scope and keeps a handful. After a
//! bare `use Test` a closure created inside a sub visits 96 keys to keep 31, and
//! 49 of the 65 rejects are decided by the key alone: `__mutsu_callable_id::`
//! routine-registration markers, one per routine the import made visible, plus
//! attribute-twigil materializations. That is the cost #7565 is about — the
//! importing scope's ordinary code gets slower in proportion to how much the
//! module exports — and it is paid on *every* closure creation.
//!
//! Those rejects cannot be memoized against the tier's *address*: an armed
//! address memo has to hold the tier's `Arc` to stop the allocator recycling it,
//! and holding it forces `Arc::make_mut` to clone the map on the next ordinary
//! value write, so in a loop that writes a mainline lexical the memo never
//! survives to be read (measured, #7565 and #8019). Hanging the memo off the
//! tier itself has neither problem: identity is structural rather than
//! positional, and a copy-on-write clone carries the memo across because a
//! clone has exactly the key set the original had.
//!
//! [`Env`]: crate::env::Env
//! [`Env::filtered_flat_capture`]: crate::env::Env::filtered_flat_capture

use std::ops::Deref;
use std::sync::OnceLock;
use std::sync::atomic::{AtomicBool, Ordering};

use crate::symbol::{Symbol, flags};
use crate::value::Value;

/// A single env tier's overlay storage.
// Keyed by interned `Symbol`; `FxHashMap` (non-cryptographic hash over the
// small integer key), not the default `SipHash` — this is the map every env
// lookup probes.
pub(crate) type SymMap = rustc_hash::FxHashMap<Symbol, Value>;

/// One env tier: the overlay map plus its key-set-derived indexes.
///
/// `Clone` carries the indexes over deliberately — a clone has the original's
/// key set, which is exactly what they describe. That is what makes them
/// survive `Arc::make_mut`'s copy-on-write.
#[derive(Default)]
pub(crate) struct Tier {
    map: SymMap,
    /// Memo of [`Self::capture_candidates`]. Empty until something asks;
    /// reset by every mutation that can ADD a key (see
    /// [`Self::invalidate_key_set`]).
    ///
    /// A *superset* index, never a subset — a removal deliberately leaves it
    /// alone, so a named key may no longer be in the map and the reader looks
    /// each one up rather than trusting it to be present. It must never MISS a
    /// present key, or a closure would silently capture one name fewer than it
    /// should; that is what the private `map` guarantees.
    ///
    /// Boxed: a `Tier` is allocated per copy-on-write clone of any frame
    /// overlay, so the memo costs those allocations one pointer rather than the
    /// index inline.
    capture_candidates: OnceLock<Box<CandidateIndex>>,
    /// Whether a capture has already walked this tier — see
    /// [`Tier::capture_walk`], which builds the memo only on the SECOND ask.
    capture_asked: AtomicBool,
}

impl Clone for Tier {
    fn clone(&self) -> Self {
        Self {
            map: self.map.clone(),
            capture_candidates: self.capture_candidates.clone(),
            capture_asked: AtomicBool::new(self.capture_asked.load(Ordering::Relaxed)),
        }
    }
}

/// The candidate list, with the map length it was built at. The length is a
/// debug-only staleness net: an addition invalidates the memo, so a longer map
/// than the one it was built from means a mutator bypassed
/// [`Tier::invalidate_key_set`].
#[derive(Clone)]
struct CandidateIndex {
    keys: Box<[Symbol]>,
    built_at_len: usize,
}

/// How [`Env::filtered_flat_capture`] should enumerate one tier — see
/// [`Tier::capture_walk`].
///
/// [`Env::filtered_flat_capture`]: crate::env::Env::filtered_flat_capture
pub(crate) enum CaptureWalk<'a> {
    /// Iterate the tier's whole map and let the filter reject what it rejects.
    Entries,
    /// Iterate these keys and look each one up, skipping the rest of the map
    /// unvisited. Worth its hash probe per key only when the list is much
    /// shorter than the map — the `use Test` case, where it is 43 keys against
    /// 92 (#7565).
    Candidates(&'a [Symbol]),
}

/// True when no closure capture can ever keep `key`, decided by the key alone —
/// the part of the capture filter that does not depend on which closure is
/// being created, factored out so a tier can memoize it once per key set.
///
/// * Attribute-twigil keys (`!x`, `@!x`, `%.x`, ...) are per-frame
///   materializations of `self`'s attributes, not lexicals: the closure must
///   read them through its captured `self` at RUN time, or a creation-time
///   snapshot goes stale the moment the instance mutates.
/// * `__mutsu_callable_id::<pkg>::<name>` is a routine-registration marker, one
///   per named routine visible in the creating scope, and nothing a closure
///   body can name. Every consumer reads it from the LIVE env at call time; the
///   single case that cannot (an escaping closure whose `use`-inside-`EVAL`
///   scope has been popped) is re-added by name in `capture_bare_callees`.
/// * `__mutsu_callable_type` is closure-IDENTITY metadata (the WhateverCode
///   marker), installed on the genuine closure's own env after capture. An
///   ordinary inner block that inherited it would be mis-detected as a
///   WhateverCode.
///
/// `__mutsu_type::<name>` is *shadow metadata*: it says what `<name>` is
/// constrained to and nothing can observe it except through `<name>` itself, so
/// it is decided on its subject's terms rather than its own (#7964).
#[inline]
pub(crate) fn capture_never_keeps(key: Symbol) -> bool {
    let mut k = key;
    let mut f = k.flags();
    if f & flags::TYPE_META != 0 {
        // `None` cannot happen (the flag is a pure string property of the
        // prefix), but keeping the key is the conservative answer.
        let Some(subject) = k.type_meta_subject() else {
            return false;
        };
        k = subject;
        f = subject.flags();
    }
    capture_never_keeps_resolved(k, f)
}

/// [`capture_never_keeps`] for a caller that has already resolved the key
/// through its `__mutsu_type::` wrapper and loaded its flags word — the closure
/// capture filter, which needs both anyway. Sharing this tail is what keeps the
/// two askers from drifting apart; drift here would mean a tier skipping a key
/// the filter would have kept, i.e. a silently incomplete closure capture.
#[inline]
pub(crate) fn capture_never_keeps_resolved(key: Symbol, flags: u16) -> bool {
    const DROP: u16 = flags::ATTR_TWIGIL_ENV_KEY | flags::CALLABLE_ID_META;
    flags & DROP != 0 || key == crate::symbol::well_known::callable_type()
}

/// Smallest tier the candidate memo is built for — see [`Tier::capture_walk`].
const CANDIDATE_MEMO_MIN_KEYS: usize = 32;

impl Tier {
    /// Wrap an already-built map. Every whole-map rebuild goes through here, so
    /// the indexes start empty exactly as they must.
    pub(crate) fn new(map: SymMap) -> Self {
        Self {
            map,
            capture_candidates: OnceLock::new(),
            capture_asked: AtomicBool::new(false),
        }
    }

    /// This tier's keys that a closure capture could keep — everything
    /// [`capture_never_keeps`] does not reject. Computed on first ask and held
    /// until the key set changes.
    pub(crate) fn capture_candidates(&self) -> &[Symbol] {
        if let Some(idx) = self.capture_candidates.get() {
            debug_assert!(
                idx.built_at_len >= self.map.len(),
                "capture-candidate memo missed a key added since it was built",
            );
            return &idx.keys;
        }
        // Pre-sized: the list is at most the map's key count, and growing a
        // `Vec` through its doubling ladder for a wide tier was a measurable
        // share of the build (`realloc` on `benchmarks/bench-ctor.raku`).
        let mut keys = Vec::with_capacity(self.map.len());
        keys.extend(
            self.map
                .keys()
                .copied()
                .filter(|k| !capture_never_keeps(*k)),
        );
        let keys = keys.into_boxed_slice();
        let built_at_len = self.map.len();
        &self
            .capture_candidates
            .get_or_init(|| Box::new(CandidateIndex { keys, built_at_len }))
            .keys
    }

    /// How a closure capture should enumerate this tier.
    ///
    /// Walking the by-key candidate list costs a hash probe per key, which only
    /// pays when the list is much shorter than the map. Everything else walks
    /// the map exactly as it did before the memo existed — the filter still
    /// rejects the never-keeps, it just does not get to skip them — so a
    /// program with no wide import list pays nothing for this.
    pub(crate) fn capture_walk(&self) -> CaptureWalk<'_> {
        // Below this, building the memo cannot pay for itself. The tiers that
        // matter are the wide ones a whole file's names live in; a call frame's
        // own overlay is a handful of entries AND is allocated fresh per call,
        // so it would rebuild the list — a `Vec` collect and its allocation —
        // on every single capture and never read it twice (measured: ~400
        // instructions per closure creation for a 4-entry frame tier, #7565).
        if self.map.len() < CANDIDATE_MEMO_MIN_KEYS {
            return CaptureWalk::Entries;
        }
        // Build on the SECOND ask, never the first — the same "wait for a
        // repeat" discipline `vm_capture_cache` arms its memo with, and for the
        // same reason. A tier can be both wide and allocated fresh per call: a
        // method frame that flattens its chain gets one, and a capture inside
        // it would build a list, read it once and drop it. That is a pure loss,
        // and it was 2.4% of `benchmarks/bench-ctor.raku` (#7565). A tier worth
        // memoizing is by definition one a second capture comes back to.
        if self.capture_candidates.get().is_none()
            && !self.capture_asked.swap(true, Ordering::Relaxed)
        {
            return CaptureWalk::Entries;
        }
        let candidates = self.capture_candidates();
        // Break-even is around two thirds: skipping a key saves an iterator
        // step and a filter call, and costs a hash probe on each key that is
        // kept. (The index is a superset, so after removals it can be as long
        // as -- or longer than -- the map; that lands on `Entries`, which is
        // right.)
        if candidates.len() * 3 >= self.map.len() * 2 {
            return CaptureWalk::Entries;
        }
        CaptureWalk::Candidates(candidates)
    }

    /// Upper bound on how many of this tier's entries a capture can keep, for
    /// pre-sizing the result map. Reads the memo if it is already there and
    /// never builds it — building is [`Self::capture_walk`]'s decision, and
    /// this runs before it.
    pub(crate) fn capture_upper_bound(&self) -> usize {
        match self.capture_candidates.get() {
            Some(idx) => idx.keys.len().min(self.map.len()),
            None => self.map.len(),
        }
    }

    /// Drop every key-set-derived index. Called by each mutator that can ADD a
    /// key; a mutator that can only REMOVE one leaves them alone, because they
    /// are supersets by contract.
    #[inline(always)]
    fn invalidate_key_set(&mut self) {
        // `take` needs no atomic: it has `&mut self`, so the initialized bit is
        // a plain read. It is reached only from a mutator that really did add a
        // key, which on the hot insert path is the rare case.
        self.capture_candidates.take();
    }

    #[inline]
    pub(crate) fn insert(&mut self, key: Symbol, value: Value) -> Option<Value> {
        let prev = self.map.insert(key, value);
        if prev.is_none() {
            self.invalidate_key_set();
        }
        prev
    }

    /// A removal cannot invalidate a superset index — see
    /// [`Self::capture_candidates`].
    #[inline]
    pub(crate) fn remove(&mut self, key: &Symbol) -> Option<Value> {
        self.map.remove(key)
    }

    /// Value-only: the key must already be present, so the key set is unchanged.
    #[inline]
    pub(crate) fn get_mut(&mut self, key: &Symbol) -> Option<&mut Value> {
        self.map.get_mut(key)
    }

    #[inline]
    pub(crate) fn values_mut(
        &mut self,
    ) -> std::collections::hash_map::ValuesMut<'_, Symbol, Value> {
        self.map.values_mut()
    }

    /// Filtering can only remove keys — superset indexes survive it.
    #[inline]
    pub(crate) fn retain(&mut self, f: impl FnMut(&Symbol, &mut Value) -> bool) {
        self.map.retain(f);
    }

    #[inline]
    pub(crate) fn reserve(&mut self, additional: usize) {
        self.map.reserve(additional);
    }

    /// Raw mutable access, for the bulk paths that add keys without naming
    /// them. Drops the indexes up front, since it cannot report what it did.
    #[inline]
    pub(crate) fn map_mut(&mut self) -> &mut SymMap {
        self.invalidate_key_set();
        &mut self.map
    }

    /// Unwrap the tier back into its map (`Env::into_iter`).
    pub(crate) fn into_map(self) -> SymMap {
        self.map
    }
}

impl Deref for Tier {
    type Target = SymMap;

    #[inline(always)]
    fn deref(&self) -> &SymMap {
        &self.map
    }
}

impl std::fmt::Debug for Tier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.map.fmt(f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn s(name: &str) -> Symbol {
        Symbol::intern(name)
    }

    fn candidates_are_exact(tier: &Tier) -> bool {
        let mut want: Vec<Symbol> = tier
            .keys()
            .copied()
            .filter(|k| !capture_never_keeps(*k))
            .collect();
        let mut got: Vec<Symbol> = tier
            .capture_candidates()
            .iter()
            .copied()
            .filter(|k| tier.contains_key(k))
            .collect();
        want.sort_by_key(|k| k.id());
        got.sort_by_key(|k| k.id());
        want == got
    }

    #[test]
    fn candidate_memo_drops_the_never_captured_families() {
        let mut tier = Tier::default();
        tier.insert(s("$x"), Value::int(1));
        tier.insert(s("__mutsu_callable_id::GLOBAL::ok"), Value::int(2));
        tier.insert(s("@!attr"), Value::int(3));
        tier.insert(s("__mutsu_callable_type"), Value::int(4));
        let got: Vec<Symbol> = tier.capture_candidates().to_vec();
        assert_eq!(got, vec![s("$x")]);
    }

    #[test]
    fn a_value_overwrite_keeps_the_memo_and_a_new_key_rebuilds_it() {
        let mut tier = Tier::default();
        tier.insert(s("$x"), Value::int(1));
        let before = tier.capture_candidates().as_ptr();
        // Value-only write: the key set is unchanged, so the memo must survive
        // -- this is the whole point of the index (#7565).
        tier.insert(s("$x"), Value::int(2));
        assert_eq!(tier.capture_candidates().as_ptr(), before);
        tier.insert(s("$y"), Value::int(3));
        assert!(candidates_are_exact(&tier));
        assert_eq!(tier.capture_candidates().len(), 2);
    }

    #[test]
    fn a_clone_carries_the_memo_because_it_has_the_same_keys() {
        let mut tier = Tier::default();
        tier.insert(s("$x"), Value::int(1));
        let _ = tier.capture_candidates();
        let copy = tier.clone();
        assert_eq!(copy.capture_candidates(), &[s("$x")]);
        assert!(candidates_are_exact(&copy));
    }

    #[test]
    fn a_removal_leaves_a_superset_the_reader_must_re_probe() {
        let mut tier = Tier::default();
        tier.insert(s("$x"), Value::int(1));
        tier.insert(s("$y"), Value::int(2));
        let _ = tier.capture_candidates();
        tier.remove(&s("$y"));
        assert_eq!(tier.capture_candidates().len(), 2);
        assert!(candidates_are_exact(&tier));
    }

    #[test]
    fn map_mut_drops_the_memo_because_it_can_add_a_key() {
        let mut tier = Tier::default();
        tier.insert(s("$x"), Value::int(1));
        let _ = tier.capture_candidates();
        tier.map_mut().insert(s("$z"), Value::int(9));
        assert!(candidates_are_exact(&tier));
        assert_eq!(tier.capture_candidates().len(), 2);
    }
}
