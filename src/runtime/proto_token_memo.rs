//! A memo for [`Interpreter::has_proto_token`] (#12917).
//!
//! The answer walks the current package's MRO and probes `proto_tokens`
//! under every ancestor's qualified name, about 10k instructions for a
//! grammar a few classes deep, and every `Grammar.parse` asks it for its
//! start rule (YAMLish concretizes each plain scalar with one
//! `Schema::Core.new.parse`). It depends on the name, the current package
//! (whose MRO an unqualified name is looked up through) and the registry: the
//! token tables and the class hierarchy, both written only through
//! `routines_mut()` / `registry_mut()`, which move the registry write
//! generation. So an answer is kept for one generation, keyed by the package
//! and the name.

use super::*;
use crate::symbol::Symbol;
use std::cell::RefCell;

thread_local! {
    /// The generation the entries were filled under, and the entries.
    static MEMO: RefCell<(u64, rustc_hash::FxHashMap<(Symbol, Symbol), bool>)> =
        RefCell::new((0, rustc_hash::FxHashMap::default()));
}

impl Interpreter {
    /// Whether `name` is declared as a proto token, looked up as
    /// [`Self::has_proto_token_uncached`] does, through the memo above.
    // Cost: O(1) expected for a name already asked under this registry
    // generation and package; otherwise as `has_proto_token_uncached`.
    pub(crate) fn has_proto_token(&self, name: &str) -> bool {
        // Every probe is into `proto_tokens`.
        if self.routines().proto_tokens.is_empty() {
            return false;
        }
        let generation = self.registry_write_generation();
        let key = (self.current_package_sym(), Symbol::intern(name));
        if let Some(hit) = MEMO.with(|memo| {
            let memo = memo.borrow();
            (memo.0 == generation).then(|| memo.1.get(&key).copied()).flatten()
        }) {
            return hit;
        }
        let answer = self.has_proto_token_uncached(name);
        MEMO.with(|memo| {
            let mut memo = memo.borrow_mut();
            if memo.0 != generation {
                memo.1.clear();
                memo.0 = generation;
            }
            memo.1.insert(key, answer);
        });
        answer
    }
}
