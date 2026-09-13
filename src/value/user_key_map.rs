//! The hasher policy for maps whose keys are *runtime data* — [`ValueMap`].
//!
//! Resolves [#8333](https://github.com/tokuhirom/mutsu/issues/8333); the
//! decision and what was rejected are
//! [ADR-0103](../../../docs/adr/0103-user-key-map-hasher.md).
//!
//! # Two populations of String-keyed map, two hashers
//!
//! mutsu already declines std's SipHash for its *internal* tables — the
//! registry maps, `SymMap`, `Env::tombstones`, the compiled-functions table —
//! and their doc comments all justify it the same way: the keys are **program
//! identifiers**, chosen by the source text, never by an attacker. `FxHash` is
//! the right answer there and those maps use it.
//!
//! `HashData::map` is the other population. Its keys are whatever the running
//! program put in a hash: a JSON object's field names, a parsed header set, a
//! CSV column. Those can be attacker-chosen, which is exactly the collision-DoS
//! threat SipHash exists to defeat, so `FxHash` — unseeded, and trivially
//! invertible — is *not* an acceptable answer here even though it is the
//! cheapest.
//!
//! [`ValueMap`] takes the third option: a fast hasher that is still **randomly
//! seeded per instance**, so an attacker cannot precompute a colliding key set
//! offline. `foldhash::fast::RandomState` is `hashbrown`'s own default hasher —
//! `std::collections::HashMap` is hashbrown, so this changes the `BuildHasher`
//! and nothing about the table algorithm — and it is already in the dependency
//! tree, adding no new audited surface.
//!
//! # Iteration order is unchanged in the only sense Raku specifies
//!
//! A user-key map's iteration order is observable (`%h.keys`, `.values`,
//! `.pairs`, `.gist`). Raku specifies it as arbitrary, and mutsu's order is
//! *already* nondeterministic run-to-run, because std's `RandomState` reseeds
//! every process:
//!
//! ```text
//! $ for i in 1 2 3; do mutsu -e 'say (a=>1,b=>2,c=>3,d=>4).hash.keys.join(",")'; done
//! c,a,d,b
//! b,d,a,c
//! a,c,b,d
//! ```
//!
//! So no test can depend on the order today, and keeping a randomly-seeded
//! hasher preserves that property exactly. This is the load-bearing reason the
//! seeded option is a non-event for the suites where `FxHash` would have been a
//! behavior change (it would make the order *deterministic*, which is a
//! different observable promise mutsu should not start making).
//!
//! # What this is not
//!
//! Not a cryptographic hash, and not a defense against an attacker who can
//! observe a long-running process's behavior closely enough to recover the
//! seed — foldhash's own docs disclaim both. The threat this closes is the
//! realistic one: a precomputed colliding key set fed in as data.

use std::collections::HashMap;

/// `BuildHasher` for [`UserKeyMap`] — see the module docs for why it is neither
/// SipHash nor `FxHash`.
pub type UserKeyState = foldhash::fast::RandomState;

/// A `String`-keyed map whose keys come from *runtime data* rather than from
/// program text. Interchangeable with `HashMap<String, V>` at every call site
/// except construction: there is no `new()` (that is std-only for
/// `RandomState`), so build one with `UserKeyMap::default()`,
/// `HashMap::with_hasher(..)`, or `collect()`.
pub type UserKeyMap<V> = HashMap<String, V, UserKeyState>;

/// The hash a Raku `%h` is: [`UserKeyMap`] of [`Value`](crate::value::Value).
pub type ValueMap = UserKeyMap<crate::value::Value>;

/// `HashMap::with_capacity` for a [`UserKeyMap`], which has no inherent one.
#[inline]
pub fn with_capacity<V>(cap: usize) -> UserKeyMap<V> {
    HashMap::with_capacity_and_hasher(cap, UserKeyState::default())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::Value;

    #[test]
    fn lookup_by_str_matches_string_keyed_behavior() {
        let mut m: ValueMap = ValueMap::default();
        m.insert("k".to_string(), Value::Int(1));
        assert_eq!(m.get("k"), Some(&Value::Int(1)));
        assert_eq!(m.get("nope"), None);
        assert_eq!(m.len(), 1);
    }

    #[test]
    fn collect_and_with_capacity_build_a_seeded_map() {
        let m: ValueMap = [("a".to_string(), Value::Int(1))].into_iter().collect();
        assert!(m.contains_key("a"));
        let mut c: ValueMap = with_capacity(8);
        c.insert("b".to_string(), Value::Int(2));
        assert!(c.capacity() >= 8);
        assert!(c.contains_key("b"));
    }

    /// The whole point of the seeded choice: two maps in the same process do not
    /// share a fixed bucket assignment, so a colliding key set cannot be
    /// precomputed. Verified through the `BuildHasher` rather than through
    /// iteration order, which is a probabilistic signal on small maps.
    #[test]
    fn state_is_randomly_seeded_per_instance() {
        use std::hash::BuildHasher;
        let (a, b) = (UserKeyState::default(), UserKeyState::default());
        let key = "some-runtime-key";
        assert_ne!(
            a.hash_one(key),
            b.hash_one(key),
            "two UserKeyState instances hashed a key identically — the seed is not random"
        );
    }
}
