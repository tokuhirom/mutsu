//! The `:=` alias keys (`__mutsu_sigilless_alias::<name>`) that have ever been
//! an env key in this process, in the order they first became one.
//!
//! A block's exit re-syncs each aliased local of its frame with the alias's
//! target. It used to ask every local of the frame whether its alias key might
//! exist ([`crate::symbol::maybe_env_key`]), O(L) per exit once any alias
//! existed anywhere (#9170). The keys that can answer yes are exactly the ones
//! logged here, so the exit visits those instead: O(a), a = alias keys the
//! program has ever bound.
//!
//! The log is appended by [`crate::symbol::mark_env_key`], the closed set of
//! sites that can put a key into an env tier, so it is complete in the same
//! sense the env-key latch is: a key not logged has never been in any env.
//! Like that latch it is monotonic and never shrinks.

use crate::symbol::Symbol;
use std::sync::RwLock;
use std::sync::atomic::{AtomicUsize, Ordering};

static LOG: RwLock<Vec<Symbol>> = RwLock::new(Vec::new());
/// `LOG.len()`, readable without the lock: zero is the common program's answer.
static LEN: AtomicUsize = AtomicUsize::new(0);

/// Log `sym`, an alias key that is becoming an env key, unless it already is.
// Cost: O(1) amortized for a first marking (the caller's bit test makes it
// once per key); O(a) for a key whose flag slot is out of reach, which the
// caller cannot dedupe and which is checked here instead.
pub(crate) fn note(sym: Symbol, deduped: bool) {
    let mut log = LOG.write().unwrap_or_else(|e| e.into_inner());
    if !deduped && log.contains(&sym) {
        return;
    }
    log.push(sym);
    LEN.store(log.len(), Ordering::Release);
}

/// Call `f` with every logged alias key, in logging order. `f` must not put a
/// key into an env (that would log under the lock it holds).
// Cost: O(a), a = alias keys ever logged.
pub(crate) fn for_each(mut f: impl FnMut(Symbol)) {
    if LEN.load(Ordering::Acquire) == 0 {
        return;
    }
    let log = LOG.read().unwrap_or_else(|e| e.into_inner());
    for sym in log.iter() {
        f(*sym);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_marked_alias_key_is_logged_once() {
        let sym = Symbol::intern("__mutsu_sigilless_alias::$alias_key_log_test");
        crate::symbol::mark_env_key(sym);
        crate::symbol::mark_env_key(sym);
        let mut seen = 0;
        for_each(|s| seen += usize::from(s == sym));
        assert_eq!(seen, 1);

        let plain = Symbol::intern("$alias_key_log_plain");
        crate::symbol::mark_env_key(plain);
        let mut logged = false;
        for_each(|s| logged |= s == plain);
        assert!(!logged, "only alias keys are logged");
    }
}
