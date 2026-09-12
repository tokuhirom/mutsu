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
//!
//! # What it does
//!
//! [`MetaNs`] enumerates the namespaces and [`MetaNs::key`] is the only way to
//! obtain a key `Symbol` for one. The `(namespace, name) -> key` mapping never
//! changes — symbols are append-only — so it is memoized per thread and the
//! `format!` runs once per pair for the life of the process.
//!
//! This deliberately replaces what used to be seven near-identical
//! `*_key_for_sym` helpers, each with its own copy of the same memo boilerplate
//! and its own thread-local table. One table, one place to add a namespace, and
//! adding the 37th is correct by construction rather than by remembering.
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
//! site instead of at twenty-eight.
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
/// Only the namespaces that have been migrated are listed. A namespace is
/// added here when its call sites move over, not in advance: an unused variant
/// is dead code, and `scripts/check-magic-keys.sh` is what tracks the ones that
/// have not moved yet.
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

    /// [`Self::key`] for a caller that only has the name as a `&str`.
    ///
    /// Interns `name` before the memo lookup, so it costs a string hash on
    /// every call and is the wrong entry point for a hot path — reach for it
    /// only where the caller genuinely has no `Symbol`.
    pub(crate) fn key_for_str(self, name: &str) -> Symbol {
        self.key(Symbol::intern(name))
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
