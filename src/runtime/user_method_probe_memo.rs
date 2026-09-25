//! A memo of [`Interpreter::grammar_has_user_method_sym`] per
//! `(class, method)`.
//!
//! The method-call preamble asks it on every call whose receiver is an
//! instance or a type object, to decide whether a user method must beat the
//! native fast paths. The answer is a pure function of the registry (the
//! class's MRO, its and its roles' method tables), yet it walked the MRO with
//! a registry probe per level on every call: ~1,200 instructions of each
//! `P.new(...)` (#9291). The memo is keyed on the registry write generation,
//! which every registry mutation bumps, so it cannot outlive a declaration.

use super::Interpreter;
use crate::symbol::Symbol;

/// The memo itself: the registry write generation it is valid for, and the
/// answers recorded under it.
#[derive(Default)]
pub(crate) struct UserMethodProbeMemo {
    generation: u64,
    answers: rustc_hash::FxHashMap<(Symbol, Symbol), bool>,
}

impl Interpreter {
    /// [`Self::grammar_has_user_method_sym`], memoized per `(class, method)`
    /// for one registry write generation.
    // Cost: O(1) on a hit; a miss costs one MRO walk, O(d), d = MRO depth.
    pub(crate) fn grammar_has_user_method_memo(&mut self, class: Symbol, method: Symbol) -> bool {
        let generation = self.registry_write_generation();
        let memo = &mut self.user_method_probe_memo;
        if memo.generation != generation {
            memo.generation = generation;
            memo.answers.clear();
        } else if let Some(&answer) = memo.answers.get(&(class, method)) {
            return answer;
        }
        let answer = self.grammar_has_user_method_sym(class.as_str(), method);
        // Record it only if resolving the answer declared nothing (a role
        // pun, say): an answer computed across a registry write is not an
        // answer for either generation.
        if self.registry_write_generation() == generation {
            self.user_method_probe_memo
                .answers
                .insert((class, method), answer);
        }
        answer
    }
}
