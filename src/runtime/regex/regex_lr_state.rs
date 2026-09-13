//! Left-recursion bookkeeping for `<subrule>` calls.
//!
//! Every `<subrule>` reference registers an *activation* for the duration of
//! its body walk, so that a re-entry of the same rule at the same position
//! reads a growing seed instead of recursing forever. The state this needs is
//! three facts per `(rule, position)` key: whether the key is under
//! evaluation, the seed it has grown so far, and whether anything actually
//! read that seed.
//!
//! Those three facts used to live in three separate thread-local maps keyed by
//! an owned `(String, usize)`, so one activation cost three `String` clones and
//! six hash-map operations, plus the same again to tear it down — measured at
//! 4.7% of a YAMLish parse for 70k activations, none of which were genuinely
//! left-recursive ([#7576](https://github.com/tokuhirom/mutsu/issues/7576)).
//! One map of one entry struct, keyed by the *interned* rule name, does the
//! same work in one hash lookup per operation and no allocation at all.
//!
//! Round 21 asks the prior question instead: does this call need an activation
//! at all? A `<subrule>` call can only ever re-enter its own key through a call
//! to a rule of the same name, and the rule call graph
//! ([`super::regex_call_graph`]) already decides whether one is reachable. When
//! it proves none is, AND no rule in the call cone runs user code that could
//! name one by hand, AND no activation of that name is live further up the
//! stack, the three map operations are provably a no-op and are skipped
//! outright. The gate itself is the memoized verdict plus one `Vec` index, in
//! the same thread-local the state lives in.

use rustc_hash::FxHashMap as HashMap;
use std::cell::RefCell;
use std::collections::hash_map::Entry;

use crate::runtime::regex_types::RegexCaptures;
use crate::symbol::Symbol;

/// The left-recursion key a `<subrule>` call at `pos` is evaluated under:
/// `(written rule name, argument identity, chars remaining)`.
///
/// It carries no package, so two grammars that define the same rule name share
/// it — see `regex_call_graph::subrule_cannot_reenter_itself`.
#[derive(Clone, PartialEq, Eq, Hash)]
pub(super) struct LrKey {
    /// The written rule name, interned. The set of rule names a program can
    /// name is bounded by its source text, so putting them in the (leaked,
    /// append-only) symbol table is bounded too.
    name: Symbol,
    /// The formatted argument values, for a parameterized call whose arguments
    /// are part of its rule identity: `multi rule expr($p)` calling
    /// `<expr($p-1)>` at the same position is ordinary recursion toward a base
    /// case, NOT left recursion (99problems-41-to-50.t P47).
    ///
    /// Deliberately NOT interned: an argument value is runtime data, and
    /// interning it would grow the symbol table without bound. `None` — the
    /// overwhelmingly common case — makes the whole key allocation-free.
    args: Option<Box<str>>,
    /// `chars.len() - pos`. Identifies "this rule at this position in the
    /// subject" regardless of how deeply the `chars` slice has been sliced.
    remaining: usize,
}

impl LrKey {
    pub(super) fn new(name: Symbol, args: Option<Box<str>>, remaining: usize) -> Self {
        LrKey {
            name,
            args,
            remaining,
        }
    }
}

/// Everything the left-recursion machinery keeps per thread.
///
/// One thread-local rather than three: `lr_begin_or_reenter` and
/// `lr_end_activation` touch the key map and the per-name activation count
/// together, and the gate reads the count and the memoized verdict together, so
/// splitting them would only buy extra [`std::thread::LocalKey`]
/// accesses on the hottest path in the matcher.
struct LrState {
    /// Per-key state for the calls currently in flight.
    keys: HashMap<LrKey, LrEntry>,
    /// How many activations of each rule NAME are live, indexed by
    /// [`Symbol::id`]. A name with no live activation cannot be re-entered by
    /// anything, which is what lets a proven-safe call skip `keys` entirely —
    /// and answering it by array index keeps the gate cheaper than the one hash
    /// lookup it replaces.
    active: Vec<u32>,
    /// The token generation [`LrState::skip`] was computed under. Rule bodies
    /// can be redefined (`TOKEN_DEFS_GEN`), which invalidates every verdict.
    generation: u64,
    /// `(package, rule name) -> this call provably needs no bookkeeping`, as
    /// decided by [`super::regex_call_graph`]'s cone walk.
    skip: HashMap<(Symbol, Symbol), bool>,
}

impl LrState {
    /// Const-constructible so the thread-local needs no lazy-initialization
    /// check on every access — this is the matcher's hottest thread-local.
    const fn new() -> Self {
        LrState {
            keys: HashMap::with_hasher(rustc_hash::FxBuildHasher),
            active: Vec::new(),
            generation: 0,
            skip: HashMap::with_hasher(rustc_hash::FxBuildHasher),
        }
    }

    #[inline]
    fn bump(&mut self, name: Symbol) {
        let idx = name.id() as usize;
        if idx >= self.active.len() {
            self.active.resize(idx + 1, 0);
        }
        self.active[idx] += 1;
    }

    #[inline]
    fn unbump(&mut self, name: Symbol) {
        if let Some(slot) = self.active.get_mut(name.id() as usize) {
            *slot = slot.saturating_sub(1);
        }
    }
}

/// One key's left-recursion state.
#[derive(Default)]
struct LrEntry {
    /// The seed matches grown so far, in HIGHEST PRIORITY FIRST order (as
    /// returned by `regex_match_ends_from_caps_in_pkg`), with absolute
    /// positions and no outer wrapping. `Some` exactly while the key is under
    /// evaluation; an empty vector means "no match yet" (the initial seed).
    seed: Option<Vec<(usize, RegexCaptures)>>,
    /// The seed was CONSULTED — read by a recursive re-entry while this key
    /// was active, i.e. the key is genuinely left-recursive at this position.
    /// Only those keys need the seed-growing loop's second iteration.
    ///
    /// Outlives the activation: `lr_end_activation` hands the flag back to the
    /// enclosing activation of the same key.
    seed_read: bool,
}

impl LrEntry {
    /// Nothing left to remember — the entry can be dropped, which is what
    /// keeps the map from accumulating one dead entry per (rule, position).
    fn is_inert(&self) -> bool {
        self.seed.is_none() && !self.seed_read
    }
}

thread_local! {
    /// Per-key left-recursion state for the calls currently in flight, the
    /// per-name activation counts, and the memoized "needs no bookkeeping"
    /// verdicts.
    ///
    /// Fx-hashed rather than SipHash-hashed: these maps are probed several
    /// times per `<subrule>` call at every position, and a grammar rule name is
    /// not adversarial input.
    static LR_STATE: RefCell<LrState> = const { RefCell::new(LrState::new()) };
}

/// `true` when this key is already being evaluated further up the stack, i.e.
/// entering it again would be a left-recursive re-entry.
pub(super) fn lr_key_is_active(key: &LrKey) -> bool {
    LR_STATE.with(|s| s.borrow().keys.get(key).is_some_and(|e| e.seed.is_some()))
}

/// Mark `key` as under evaluation with an empty seed, returning the enclosing
/// activation's "seed was consulted" flag for [`lr_end_activation`] to restore.
pub(super) fn lr_begin_activation(key: &LrKey) -> bool {
    LR_STATE.with(|s| {
        let mut s = s.borrow_mut();
        s.bump(key.name);
        let entry = s.keys.entry(key.clone()).or_default();
        entry.seed = Some(Vec::new());
        std::mem::take(&mut entry.seed_read)
    })
}

/// The gate [`super::regex_call_graph`] decides and this module caches: may a
/// `<name>` call in `pkg` skip its activation entirely?
///
/// `None` means "not decided for this token generation" and asks the caller to
/// run the cone walk and report back through [`lr_record_skip_verdict`]. A live
/// activation of the same NAME answers `Some(false)` outright, whatever the
/// walk says: the verdict is about what this call's own cone can reach, and an
/// *enclosing* call of the same name is not in it (two grammars that define the
/// same rule name share a key, while the walk is per package). That case keeps
/// the full bookkeeping, so the skip is observationally identical to taking it.
#[inline]
pub(super) fn lr_skip_verdict(name: Symbol, pkg: Symbol, generation: u64) -> Option<bool> {
    LR_STATE.with(|s| {
        let s = s.borrow();
        if s.active.get(name.id() as usize).is_some_and(|&n| n > 0) {
            return Some(false);
        }
        if s.generation != generation {
            return None;
        }
        s.skip.get(&(pkg, name)).copied()
    })
}

/// Record what the cone walk decided, discarding every verdict from an earlier
/// token generation.
pub(super) fn lr_record_skip_verdict(name: Symbol, pkg: Symbol, generation: u64, verdict: bool) {
    LR_STATE.with(|s| {
        let mut s = s.borrow_mut();
        if s.generation != generation {
            s.generation = generation;
            s.skip.clear();
        }
        s.skip.insert((pkg, name), verdict);
    });
}

/// What a `<subrule>` call found when it asked to start an activation.
pub(super) enum LrBegin {
    /// The key is already under evaluation further up the stack: this call is
    /// a left-recursive re-entry and gets the seed grown so far (highest
    /// priority first, raw inner matches at absolute positions) instead of
    /// recursing. Asking for it is what records the key as genuinely
    /// left-recursive, so the owner keeps growing the seed.
    Reentry(Vec<(usize, RegexCaptures)>),
    /// The activation was started; the payload is the enclosing activation's
    /// "seed was consulted" flag for [`lr_end_activation`] to restore.
    Began(bool),
}

/// [`lr_key_is_active`] + the seed read / [`lr_begin_activation`] as ONE map
/// operation.
///
/// Every `<subrule>` call asks both questions back to back and acts on exactly
/// one of them, so asking them separately hashed and probed the same key twice
/// per call — 87k redundant probes on a 60-row YAMLish parse, none of whose
/// rules are left-recursive at all. The two spellings are otherwise identical:
/// the separate `lr_key_is_active` declined to create a vacant entry, but the
/// `lr_begin_activation` that always followed it created one anyway.
pub(super) fn lr_begin_or_reenter(key: &LrKey) -> LrBegin {
    LR_STATE.with(|s| {
        let mut s = s.borrow_mut();
        let entry = s.keys.entry(key.clone()).or_default();
        if let Some(seed) = entry.seed.as_ref() {
            entry.seed_read = true;
            return LrBegin::Reentry(seed.clone());
        }
        entry.seed = Some(Vec::new());
        let outer_seed_read = std::mem::take(&mut entry.seed_read);
        s.bump(key.name);
        LrBegin::Began(outer_seed_read)
    })
}

/// Undo [`lr_begin_activation`], reporting whether anything re-entered `key`
/// and read its seed while it was active.
pub(super) fn lr_end_activation(key: &LrKey, outer_seed_read: bool) -> bool {
    LR_STATE.with(|s| {
        let mut s = s.borrow_mut();
        s.unbump(key.name);
        match s.keys.entry(key.clone()) {
            Entry::Occupied(mut occupied) => {
                let entry = occupied.get_mut();
                entry.seed = None;
                let consulted = entry.seed_read;
                entry.seed_read = outer_seed_read;
                if entry.is_inert() {
                    occupied.remove();
                }
                consulted
            }
            Entry::Vacant(vacant) => {
                if outer_seed_read {
                    vacant.insert(LrEntry {
                        seed: None,
                        seed_read: true,
                    });
                }
                false
            }
        }
    })
}

/// Has anything read `key`'s seed?
pub(super) fn lr_seed_was_consulted(key: &LrKey) -> bool {
    LR_STATE.with(|s| s.borrow().keys.get(key).is_some_and(|e| e.seed_read))
}

/// Replace the seed of the activation that owns `key` (one iteration of the
/// growing-seed loop).
pub(super) fn lr_store_seed(key: &LrKey, seed: Vec<(usize, RegexCaptures)>) {
    LR_STATE.with(|s| {
        s.borrow_mut().keys.entry(key.clone()).or_default().seed = Some(seed);
    });
}

#[cfg(test)]
mod tests {
    use super::*;

    fn key(name: &str, remaining: usize) -> LrKey {
        LrKey::new(Symbol::intern(name), None, remaining)
    }

    #[test]
    fn activation_round_trip_leaves_no_entry() {
        let k = key("lr_round_trip", 7);
        assert!(!lr_key_is_active(&k));
        let outer = lr_begin_activation(&k);
        assert!(!outer);
        assert!(lr_key_is_active(&k));
        assert!(!lr_end_activation(&k, outer));
        assert!(!lr_key_is_active(&k));
        LR_STATE.with(|s| assert!(!s.borrow().keys.contains_key(&k)));
    }

    #[test]
    fn seed_read_is_reported_once_and_handed_back_to_the_enclosing_activation() {
        let k = key("lr_seed_read", 3);
        // Outer activation, re-entered and consulted.
        let outer = lr_begin_activation(&k);
        assert!(matches!(lr_begin_or_reenter(&k), LrBegin::Reentry(seed) if seed.is_empty()));
        assert!(lr_seed_was_consulted(&k));
        // A nested activation of the same key starts un-consulted, and hands
        // the outer flag back on the way out.
        let inner = lr_begin_activation(&k);
        assert!(
            inner,
            "the outer activation's consulted flag is handed over"
        );
        assert!(!lr_seed_was_consulted(&k));
        assert!(!lr_end_activation(&k, inner));
        assert!(lr_seed_was_consulted(&k));
        assert!(lr_end_activation(&k, outer));
        LR_STATE.with(|s| assert!(!s.borrow().keys.contains_key(&k)));
    }

    #[test]
    fn stored_seed_is_returned_to_a_re_entry() {
        let k = key("lr_stored_seed", 5);
        let outer = lr_begin_activation(&k);
        lr_store_seed(&k, vec![(9, RegexCaptures::default())]);
        let LrBegin::Reentry(seed) = lr_begin_or_reenter(&k) else {
            panic!("an active key re-enters rather than beginning again");
        };
        assert_eq!(seed.len(), 1);
        assert_eq!(seed[0].0, 9);
        lr_end_activation(&k, outer);
        // The seed dies with the activation: the next call BEGINS one rather
        // than re-entering, and what it then hands a re-entry is empty again.
        assert!(matches!(lr_begin_or_reenter(&k), LrBegin::Began(_)));
        assert!(matches!(lr_begin_or_reenter(&k), LrBegin::Reentry(seed) if seed.is_empty()));
        lr_end_activation(&k, false);
        LR_STATE.with(|s| assert!(!s.borrow().keys.contains_key(&k)));
    }

    #[test]
    fn a_live_activation_of_the_name_closes_the_skip_gate() {
        let name = Symbol::intern("lr_gate_name");
        let pkg = Symbol::intern("lr_gate_pkg");
        // Undecided until someone runs the cone walk.
        assert_eq!(lr_skip_verdict(name, pkg, 1), None);
        lr_record_skip_verdict(name, pkg, 1, true);
        assert_eq!(lr_skip_verdict(name, pkg, 1), Some(true));

        // An activation of the same NAME — at any position, in any package —
        // makes the key re-enterable from outside this call's own cone, which
        // is the one thing the walk cannot see. The gate closes while it lives.
        let k = key("lr_gate_name", 11);
        let outer = lr_begin_activation(&k);
        assert_eq!(lr_skip_verdict(name, pkg, 1), Some(false));
        lr_end_activation(&k, outer);
        assert_eq!(lr_skip_verdict(name, pkg, 1), Some(true));

        // A new token generation retires every verdict.
        assert_eq!(lr_skip_verdict(name, pkg, 2), None);
        lr_record_skip_verdict(name, pkg, 2, false);
        assert_eq!(lr_skip_verdict(name, pkg, 2), Some(false));
    }

    #[test]
    fn a_reentry_does_not_count_as_a_second_activation() {
        // `lr_begin_or_reenter` bumps the name count only on the branch that
        // actually begins an activation: the re-entry branch returns the seed
        // and its caller never calls `lr_end_activation`, so counting it would
        // leak a live activation and wedge the gate shut for the rest of the
        // run.
        let name = Symbol::intern("lr_gate_reentry");
        let pkg = Symbol::intern("lr_gate_pkg2");
        let k = LrKey::new(name, None, 4);
        lr_record_skip_verdict(name, pkg, 1, true);
        let outer = lr_begin_activation(&k);
        assert!(matches!(lr_begin_or_reenter(&k), LrBegin::Reentry(_)));
        lr_end_activation(&k, outer);
        assert_eq!(lr_skip_verdict(name, pkg, 1), Some(true));
    }

    #[test]
    fn arguments_are_part_of_the_key() {
        let name = Symbol::intern("lr_args");
        let bare = LrKey::new(name, None, 4);
        let with_args = LrKey::new(name, Some("\u{0}1".into()), 4);
        let outer = lr_begin_activation(&bare);
        assert!(lr_key_is_active(&bare));
        assert!(!lr_key_is_active(&with_args));
        lr_end_activation(&bare, outer);
    }
}
