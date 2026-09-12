//! The one constructor for a `__mutsu_*` metadata env key (issue #8087).
//!
//! # Why this type exists
//!
//! A large amount of per-binding metadata — is this name a sigilless alias, is
//! it `:=`-bound, does it carry a type or key-type constraint, is it a `state`
//! variable, is its array shaped — is not stored *on* the binding. It is stored
//! as a sibling entry in the same string-keyed [`Env`](crate::env::Env), under a
//! key derived from the variable's own name. So the only way to get from a
//! binding to its own metadata is to build that derived name and probe the env
//! with it, and the obvious way to build it is `format!`.
//!
//! That has been the profiling finding in **five separate perf campaigns**:
//! the `bench-ctor` atomic-lane probe; `Digest::RIPEMD` ([#7571]), where
//! rebuilding unchanging key strings per closure call was ~3.2% of the run;
//! the typed-lexical probes ([#7766]), probed up to seven times per store;
//! `bench-ctor` name re-derivation, at *146 name interns per constructed
//! object*; and the element store ([#8069]), at **22 interns and 8 heap
//! allocations per `@a[$i] = $v`**. Each was fixed by memoizing the one key the
//! profile happened to walk through, which is why it kept coming back: the
//! memoized helpers existed, and 96 call sites still built their key by hand.
//! Those are gone now — every namespace that had a memoized replacement has
//! had its hand-built sites converted, taking the ratchet's count from 276 to
//! 174 — but 174 sites across the other namespaces still build a key the old
//! way, which is what the ratchet holds down while they are worked through.
//!
//! # What it does
//!
//! [`MetaNs`] enumerates the namespaces and [`MetaNs::key`] is the only way to
//! obtain a key `Symbol` for one. The `(namespace, name) -> key` mapping never
//! changes — symbols are append-only — so it is memoized per thread and the
//! `format!` runs once per pair for the life of the process.
//!
//! This deliberately replaces what used to be seven near-identical
//! `*_key_for_sym` helpers plus `shared_store::atomic_lane_key`'s own pair of
//! tables, each with its own copy of the same memo boilerplate. Two tables now
//! — one for the single-name namespaces, one for [`MetaNs::CallableId`]'s
//! `(package, name)` pairs — one place to add a namespace, and adding the 37th
//! is correct by construction rather than by remembering.
//!
//! # What it is NOT
//!
//! It is **not the fix**. The fix is for these keys to stop existing: every one
//! of them is a property of a single binding and belongs on that binding's
//! resolved descriptor ([#8069] §4.1) or off the per-frame env entirely
//! ([#7817] / ADR-0084). Memoizing the construction makes the current design
//! cheap, which is a smaller good than deleting it — so treat this as the
//! staging ground for that move, not as the destination. With every access
//! funnelled through one enum, retiring a namespace becomes a change at one
//! site instead of at twenty-eight — which is now literally true for the
//! namespaces below: none of them has a hand-built site left anywhere in `src/`.
//!
//! `scripts/check-magic-keys.sh` is the ratchet that keeps the remaining
//! hand-built sites from growing back while that work happens.
//!
//! [#7571]: https://github.com/tokuhirom/mutsu/issues/7571
//! [#7766]: https://github.com/tokuhirom/mutsu/issues/7766
//! [#7817]: https://github.com/tokuhirom/mutsu/issues/7817
//! [#8069]: https://github.com/tokuhirom/mutsu/issues/8069
//! [#8087]: https://github.com/tokuhirom/mutsu/issues/8087

use crate::symbol::Symbol;

/// A `__mutsu_*` per-binding metadata namespace.
///
/// Only the namespaces that have been migrated are listed, and each one here is
/// migrated *completely*: no `format!("__mutsu_<ns>::…")` for any of these is
/// left in `src/`. A namespace is added when its call sites move over, not in
/// advance — an unused variant is dead code, and `scripts/check-magic-keys.sh`
/// is what tracks the namespaces that have not moved yet.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub(crate) enum MetaNs {
    /// `__mutsu_sigilless_alias::<name>` — the `@`/`%`/`$` name a sigilless
    /// binding (`my \h = %a`) actually aliases.
    SigillessAlias,
    /// `__mutsu_sigilless_readonly::<name>` — whether a sigilless binding
    /// refuses assignment.
    SigillessReadonly,
    /// `__mutsu_type::<name>` — a lexical's declared type constraint.
    Type,
    /// `__mutsu_hash_key_type::<name>` — an object hash's key type
    /// (`my %h{Int}`).
    HashKeyType,
    /// `__mutsu_state_key::<name>` — the shared cell a `state` variable
    /// resolves to.
    State,
    /// `__mutsu_bound::<name>` — the name was installed by `:=`, which makes a
    /// readonly container mutable in place (as against a `constant`).
    Bound,
    /// `__mutsu_bound_index::<name>` — an element of this container is itself
    /// `:=`-bound, so an element store must not replace the container.
    BoundIndex,
    /// `__mutsu_shaped_array_dims::<name>` — the declared dimensions of a
    /// shaped array (`my @a[4;4]`).
    ShapedArrayDims,
    /// `__mutsu_atomic_arr::<name>` — the cross-thread lane a shared `@a`
    /// resolves to in the shared store.
    AtomicArr,
    /// `__mutsu_atomic_hash::<name>` — the lane for a shared `%h`.
    AtomicHash,
    /// `__mutsu_callable_id::<package>::<name>` — a routine's registration
    /// clone id. The one two-part namespace; built with [`MetaNs::key_pair`].
    CallableId,
}

impl MetaNs {
    /// The literal key prefix, including the trailing `::`.
    ///
    /// This is the ONLY place these strings are spelled out. `Symbol`'s own
    /// [`TYPE_META_PREFIX`](crate::symbol::TYPE_META_PREFIX) is reused rather
    /// than respelled, because `Symbol::type_meta_subject` parses keys back
    /// with it and the two must not drift.
    pub(crate) const fn prefix(self) -> &'static str {
        match self {
            MetaNs::SigillessAlias => "__mutsu_sigilless_alias::",
            MetaNs::SigillessReadonly => "__mutsu_sigilless_readonly::",
            MetaNs::Type => crate::symbol::TYPE_META_PREFIX,
            MetaNs::HashKeyType => "__mutsu_hash_key_type::",
            MetaNs::State => "__mutsu_state_key::",
            MetaNs::Bound => "__mutsu_bound::",
            MetaNs::BoundIndex => "__mutsu_bound_index::",
            MetaNs::ShapedArrayDims => "__mutsu_shaped_array_dims::",
            MetaNs::AtomicArr => "__mutsu_atomic_arr::",
            MetaNs::AtomicHash => "__mutsu_atomic_hash::",
            MetaNs::CallableId => "__mutsu_callable_id::",
        }
    }

    /// The atomic lane namespace for a container: `%h`'s if `hash_lane`, `@a`'s
    /// otherwise.
    pub(crate) const fn atomic_lane(hash_lane: bool) -> MetaNs {
        if hash_lane {
            MetaNs::AtomicHash
        } else {
            MetaNs::AtomicArr
        }
    }

    /// This namespace's env key for `name`, as a pre-interned [`Symbol`].
    ///
    /// Memoized per `(namespace, name)` in one thread-local table, so the
    /// `format!` and the string hash behind `Symbol::intern` happen once per
    /// pair for the life of the thread. Callers on a hot path should hold the
    /// name as a `Symbol` already (a compiled `SetLocal` slot, a `SetGlobal`
    /// constant, a bound parameter all do) and probe with `Env::get_sym` /
    /// `Env::contains_key_sym`, so that no string is hashed at all.
    pub(crate) fn key(self, name: Symbol) -> Symbol {
        thread_local! {
            static KEYS: std::cell::RefCell<rustc_hash::FxHashMap<(MetaNs, Symbol), Symbol>> =
                std::cell::RefCell::new(rustc_hash::FxHashMap::default());
        }
        let pair = (self, name);
        if let Some(sym) = KEYS.with(|c| c.borrow().get(&pair).copied()) {
            return sym;
        }
        let prefix = self.prefix();
        let sym = name.with_str(|n| Symbol::intern(&format!("{prefix}{n}")));
        KEYS.with(|c| {
            c.borrow_mut().insert(pair, sym);
        });
        sym
    }

    /// This namespace's env key for a two-part name (`<outer>::<inner>`), as a
    /// pre-interned [`Symbol`].
    ///
    /// Only [`MetaNs::CallableId`] is shaped this way: a routine is identified
    /// by its package *and* its name. Memoized per `(namespace, outer, inner)`
    /// exactly like [`Self::key`] — every named compiled call probes this key on
    /// entry, and building it by hand cost a `format!` of a ~40-byte string plus
    /// a hash of those bytes to intern it, per call, for a mapping fixed for the
    /// life of the routine (#7573).
    pub(crate) fn key_pair(self, outer: Symbol, inner: Symbol) -> Symbol {
        thread_local! {
            static PAIR_KEYS: std::cell::RefCell<
                rustc_hash::FxHashMap<(MetaNs, Symbol, Symbol), Symbol>,
            > = std::cell::RefCell::new(rustc_hash::FxHashMap::default());
        }
        let triple = (self, outer, inner);
        if let Some(sym) = PAIR_KEYS.with(|c| c.borrow().get(&triple).copied()) {
            return sym;
        }
        let prefix = self.prefix();
        let sym =
            outer.with_str(|o| inner.with_str(|i| Symbol::intern(&format!("{prefix}{o}::{i}"))));
        PAIR_KEYS.with(|c| {
            c.borrow_mut().insert(triple, sym);
        });
        sym
    }

    /// [`Self::key_pair`] for a caller that has both halves as `&str`.
    pub(crate) fn key_pair_for_strs(self, outer: &str, inner: &str) -> Symbol {
        self.key_pair(Symbol::intern(outer), Symbol::intern(inner))
    }

    /// [`Self::key`] for a caller that only has the name as a `&str`.
    ///
    /// Interns `name` before the memo lookup, so it costs a string hash on
    /// every call and is the wrong entry point for a hot path — reach for it
    /// only where the caller genuinely has no `Symbol`.
    pub(crate) fn key_for_str(self, name: &str) -> Symbol {
        self.key(Symbol::intern(name))
    }

    /// [`Self::key_for_str`] as a `&'static str`, for the sites that hand the
    /// key to a by-name API (the shared store is keyed by `&str`, not by
    /// `Symbol`).
    ///
    /// Still worth going through the memo: resolving an interned `Symbol` back
    /// to its `&'static str` is a thread-local array read, where the `format!`
    /// it replaces was a heap allocation, the whole `core::fmt` machinery and a
    /// matching free on every access.
    pub(crate) fn str_key_for_str(self, name: &str) -> &'static str {
        self.key_for_str(name).as_str()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_key_is_the_prefix_joined_to_the_name() {
        let key = MetaNs::SigillessAlias.key(Symbol::intern("@a"));
        assert_eq!(key.as_str(), "__mutsu_sigilless_alias::@a");
        assert_eq!(
            MetaNs::Type.key_for_str("$x").as_str(),
            "__mutsu_type::$x",
            "the Type namespace must agree with symbol::TYPE_META_PREFIX, which \
             Symbol::type_meta_subject parses keys back with"
        );
    }

    #[test]
    fn the_memo_returns_the_same_symbol_and_namespaces_do_not_collide() {
        let name = Symbol::intern("%h");
        assert_eq!(MetaNs::Bound.key(name), MetaNs::Bound.key(name));
        assert_ne!(
            MetaNs::Bound.key(name),
            MetaNs::BoundIndex.key(name),
            "the memo is keyed by (namespace, name); a name-only key would \
             hand `__mutsu_bound_index::` the `__mutsu_bound::` entry"
        );
    }

    /// The exact key every namespace produces, spelled out.
    ///
    /// A prefix typo is invisible at the type level and nearly invisible at
    /// runtime: the writer stores under one key, the reader probes another, and
    /// the metadata is simply never found — no error, just a lost constraint or
    /// a lost alias. Each line below is the spelling that the hand-built
    /// `format!` sites this type replaced produced (#8087), so the table cannot
    /// drift away from the keys already sitting in an env.
    #[test]
    fn every_namespace_spells_its_key_exactly_as_the_format_sites_did() {
        let n = Symbol::intern("@a");
        for (ns, expected) in [
            (MetaNs::SigillessAlias, "__mutsu_sigilless_alias::@a"),
            (MetaNs::SigillessReadonly, "__mutsu_sigilless_readonly::@a"),
            (MetaNs::Type, "__mutsu_type::@a"),
            (MetaNs::HashKeyType, "__mutsu_hash_key_type::@a"),
            (MetaNs::State, "__mutsu_state_key::@a"),
            (MetaNs::Bound, "__mutsu_bound::@a"),
            (MetaNs::BoundIndex, "__mutsu_bound_index::@a"),
            (MetaNs::ShapedArrayDims, "__mutsu_shaped_array_dims::@a"),
            (MetaNs::AtomicArr, "__mutsu_atomic_arr::@a"),
            (MetaNs::AtomicHash, "__mutsu_atomic_hash::@a"),
        ] {
            assert_eq!(ns.key(n).as_str(), expected, "{ns:?} key spelling");
        }
        assert_eq!(
            MetaNs::CallableId.key_pair_for_strs("GLOBAL", "f").as_str(),
            "__mutsu_callable_id::GLOBAL::f",
            "the two-part namespace joins its halves with `::` too"
        );
    }

    #[test]
    fn a_pair_key_is_memoized_per_both_halves() {
        let (a, b, f) = (
            Symbol::intern("A"),
            Symbol::intern("B"),
            Symbol::intern("f"),
        );
        assert_eq!(
            MetaNs::CallableId.key_pair(a, f),
            MetaNs::CallableId.key_pair(a, f)
        );
        assert_ne!(
            MetaNs::CallableId.key_pair(a, f),
            MetaNs::CallableId.key_pair(b, f),
            "two packages' same-named routines must not share one id key"
        );
        // `A::x` + `y` and `A` + `x::y` both flatten to the same string; the
        // memo must still hold them apart by the pair it was keyed with, and
        // the flattened key is what the env sees either way.
        assert_eq!(
            MetaNs::CallableId.key_pair_for_strs("A::x", "y"),
            MetaNs::CallableId.key_pair_for_strs("A", "x::y"),
            "the key is the flattened string, so an ambiguous split collides \
             in the env by construction -- pinned so a future namespace is not \
             given a separator that makes that ambiguity load-bearing"
        );
    }

    #[test]
    fn the_atomic_lane_selector_picks_the_matching_namespace() {
        assert_eq!(MetaNs::atomic_lane(false), MetaNs::AtomicArr);
        assert_eq!(MetaNs::atomic_lane(true), MetaNs::AtomicHash);
        assert_eq!(
            MetaNs::AtomicArr.str_key_for_str("@a"),
            "__mutsu_atomic_arr::@a",
            "the &str form must resolve to the same spelling the Symbol does"
        );
    }

    #[test]
    fn every_prefix_is_distinct_and_well_formed() {
        let all = [
            MetaNs::SigillessAlias,
            MetaNs::SigillessReadonly,
            MetaNs::Type,
            MetaNs::HashKeyType,
            MetaNs::State,
            MetaNs::Bound,
            MetaNs::BoundIndex,
            MetaNs::ShapedArrayDims,
            MetaNs::AtomicArr,
            MetaNs::AtomicHash,
            MetaNs::CallableId,
        ];
        let mut seen = std::collections::HashSet::new();
        for ns in all {
            let p = ns.prefix();
            assert!(p.starts_with("__mutsu_"), "{ns:?} prefix is not internal");
            assert!(p.ends_with("::"), "{ns:?} prefix must end with `::`");
            assert!(seen.insert(p), "{ns:?} duplicates another prefix");
        }
        // `__mutsu_bound::` is a strict prefix of nothing here, but
        // `__mutsu_bound_index::` would collide with it under a `starts_with`
        // probe. Nothing probes by prefix today; pin it so a future one cannot
        // be written against an ambiguous table.
        assert!(
            !MetaNs::BoundIndex
                .prefix()
                .starts_with(MetaNs::Bound.prefix()),
            "a namespace prefix must not be a prefix of another"
        );
    }
}
