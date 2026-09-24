//! Reverse index of sigilless / `:=` alias env entries.
//!
//! An alias is recorded in env as `__mutsu_sigilless_alias::<var> =
//! Str(<target>)`: "`<var>` is bound to `<target>`". A store to `<target>`
//! must reach every `<var>` bound to it (`my $c := $_; $_ = 5` leaves `$c` at
//! 5), which used to be found by scanning *every* entry of the running frame's
//! env overlay for an alias key naming the target -- O(env) per `SetGlobal`,
//! whether or not the program ever made an alias (#9169).
//!
//! This index answers the same question in O(aliases of the target). It is
//! filled at the one closed funnel every env key passes through,
//! [`crate::env_tier::Tier`], so no alias-creating site can slip past it.
//!
//! It is a process-global, append-only **superset**: an entry is never
//! removed, even after its alias key leaves every env or is repointed at
//! another target. A reader must therefore re-check the live env entry for
//! each candidate (one hash probe) before acting on it; the index only says
//! which names are worth probing. Over-reporting costs a probe; under-reporting
//! would lose an alias write, which the funnel placement rules out.

use crate::symbol::Symbol;
use rustc_hash::FxHashMap;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{OnceLock, PoisonError, RwLock};

type Index = FxHashMap<Symbol, Vec<Symbol>>;

/// Set once the first alias entry is registered. Until then every lookup is a
/// single relaxed load.
static ANY_ALIAS: AtomicBool = AtomicBool::new(false);

fn index() -> &'static RwLock<Index> {
    static INDEX: OnceLock<RwLock<Index>> = OnceLock::new();
    INDEX.get_or_init(|| RwLock::new(Index::default()))
}

/// Record the alias entry `key = value`, where `key` is a
/// `__mutsu_sigilless_alias::<var>` symbol. A value that is not a string names
/// no target and is ignored.
// Cost: O(1) amortized when the pair is already known (a read-locked probe);
// O(k) for a new pair, k = aliases already recorded for the same target.
pub(crate) fn note_alias_entry(key: Symbol, value: &crate::value::Value) {
    let Some(target) = value.as_str() else {
        return;
    };
    let Some(var) = key
        .as_str()
        .strip_prefix(crate::symbol::SIGILLESS_ALIAS_KEY_PREFIX)
    else {
        return;
    };
    let target = Symbol::intern(target);
    let var = Symbol::intern(var);
    if index()
        .read()
        .unwrap_or_else(PoisonError::into_inner)
        .get(&target)
        .is_some_and(|vars| vars.contains(&var))
    {
        return;
    }
    let mut idx = index().write().unwrap_or_else(PoisonError::into_inner);
    let vars = idx.entry(target).or_default();
    if !vars.contains(&var) {
        vars.push(var);
    }
    ANY_ALIAS.store(true, Ordering::Release);
}

/// Every variable ever recorded as aliased to `target` (a superset -- see the
/// module docs). Empty, without taking the lock, until any alias exists.
// Cost: O(1) when no alias exists; otherwise O(k), k = names recorded for
// `target` (copied out of the lock).
pub(crate) fn aliases_of(target: Symbol) -> Vec<Symbol> {
    if !ANY_ALIAS.load(Ordering::Acquire) {
        return Vec::new();
    }
    index()
        .read()
        .unwrap_or_else(PoisonError::into_inner)
        .get(&target)
        .cloned()
        .unwrap_or_default()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::Value;

    #[test]
    fn records_and_answers_a_superset() {
        let key = Symbol::intern("__mutsu_sigilless_alias::alias-index-test-var");
        let target = Symbol::intern("alias-index-test-target");
        note_alias_entry(key, &Value::str("alias-index-test-target".to_string()));
        // Recording the same pair twice keeps one entry.
        note_alias_entry(key, &Value::str("alias-index-test-target".to_string()));
        let vars = aliases_of(target);
        assert_eq!(
            vars.iter()
                .filter(|v| v.as_str() == "alias-index-test-var")
                .count(),
            1
        );
        // A non-string value names no target.
        note_alias_entry(key, &Value::TRUE);
        assert!(aliases_of(Symbol::intern("alias-index-test-unrelated")).is_empty());
    }
}
