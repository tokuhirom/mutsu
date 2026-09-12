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
    /// Per-key left-recursion state for the calls currently in flight.
    ///
    /// Fx-hashed rather than SipHash-hashed: this map is probed several times
    /// per `<subrule>` call at every position, and a grammar rule name is not
    /// adversarial input.
    static LR_STATE: RefCell<HashMap<LrKey, LrEntry>> = RefCell::new(HashMap::default());
}

/// `true` when this key is already being evaluated further up the stack, i.e.
/// entering it again would be a left-recursive re-entry.
pub(super) fn lr_key_is_active(key: &LrKey) -> bool {
    LR_STATE.with(|s| s.borrow().get(key).is_some_and(|e| e.seed.is_some()))
}

/// Mark `key` as under evaluation with an empty seed, returning the enclosing
/// activation's "seed was consulted" flag for [`lr_end_activation`] to restore.
pub(super) fn lr_begin_activation(key: &LrKey) -> bool {
    LR_STATE.with(|s| {
        let mut s = s.borrow_mut();
        let entry = s.entry(key.clone()).or_default();
        entry.seed = Some(Vec::new());
        std::mem::take(&mut entry.seed_read)
    })
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
        let entry = s.entry(key.clone()).or_default();
        if let Some(seed) = entry.seed.as_ref() {
            entry.seed_read = true;
            return LrBegin::Reentry(seed.clone());
        }
        entry.seed = Some(Vec::new());
        LrBegin::Began(std::mem::take(&mut entry.seed_read))
    })
}

/// Undo [`lr_begin_activation`], reporting whether anything re-entered `key`
/// and read its seed while it was active.
pub(super) fn lr_end_activation(key: &LrKey, outer_seed_read: bool) -> bool {
    LR_STATE.with(|s| {
        let mut s = s.borrow_mut();
        match s.entry(key.clone()) {
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
    LR_STATE.with(|s| s.borrow().get(key).is_some_and(|e| e.seed_read))
}

/// Replace the seed of the activation that owns `key` (one iteration of the
/// growing-seed loop).
pub(super) fn lr_store_seed(key: &LrKey, seed: Vec<(usize, RegexCaptures)>) {
    LR_STATE.with(|s| {
        s.borrow_mut().entry(key.clone()).or_default().seed = Some(seed);
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
        LR_STATE.with(|s| assert!(!s.borrow().contains_key(&k)));
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
        LR_STATE.with(|s| assert!(!s.borrow().contains_key(&k)));
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
        LR_STATE.with(|s| assert!(!s.borrow().contains_key(&k)));
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
