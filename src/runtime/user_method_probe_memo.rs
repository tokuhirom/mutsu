//! Memos of the per-`(class, method)` MRO probes on the method-call path.
//!
//! The method-call preamble asks several questions on every call whose
//! receiver is an instance or a type object: does a user method beat the
//! native fast paths ([`Interpreter::grammar_has_user_method_sym`]), does a
//! user method or a public attribute accessor win the MRO race
//! ([`Interpreter::resolve_user_method_or_accessor_sym`]), which class
//! declares the winning accessor, which MRO levels declare the method at all
//! ([`Interpreter::resolve_method_with_owner`]), does any user method exist at all
//! ([`Interpreter::has_user_method_sym`]), is it a native method
//! ([`Interpreter::is_native_method`]). Each answer is a pure function of
//! the registry (the class's MRO, its and its roles' method and attribute
//! tables), yet each walked the MRO with a registry probe per level on every
//! call: ~1,200 instructions of each `P.new(...)` (#9291), and ~100k per
//! `.m` call through a 320-deep class chain (#9172), where Rakudo's method
//! cache answers in O(1).
//!
//! Every memo is keyed on the registry write generation, which every registry
//! mutation bumps (`Interpreter::registry_mut` is the only write path; see
//! `numeric_bridge_probe.rs`), so no answer can outlive a declaration.

use super::Interpreter;
use crate::runtime::UserMethodOrAccessor;
use crate::symbol::Symbol;
use rustc_hash::FxHashMap;
use std::sync::Arc;

/// The memo itself: the registry write generation it is valid for, and the
/// answers recorded under it.
#[derive(Default)]
pub(crate) struct UserMethodProbeMemo {
    generation: u64,
    grammar_has_user_method: FxHashMap<(Symbol, Symbol), bool>,
    has_user_method: FxHashMap<(Symbol, Symbol), bool>,
    is_native_method: FxHashMap<(Symbol, Symbol), bool>,
    method_or_accessor: FxHashMap<(Symbol, Symbol), Option<UserMethodOrAccessor>>,
    public_accessor_owner: FxHashMap<(Symbol, Symbol), Option<Symbol>>,
    method_candidate_levels: FxHashMap<(Symbol, Symbol), Arc<[u32]>>,
}

/// Selects one of the memo's answer tables.
trait ProbeTable {
    type Answer: Clone;
    fn table(memo: &mut UserMethodProbeMemo) -> &mut FxHashMap<(Symbol, Symbol), Self::Answer>;
}

struct GrammarHasUserMethod;
impl ProbeTable for GrammarHasUserMethod {
    type Answer = bool;
    fn table(memo: &mut UserMethodProbeMemo) -> &mut FxHashMap<(Symbol, Symbol), bool> {
        &mut memo.grammar_has_user_method
    }
}

struct HasUserMethod;
impl ProbeTable for HasUserMethod {
    type Answer = bool;
    fn table(memo: &mut UserMethodProbeMemo) -> &mut FxHashMap<(Symbol, Symbol), bool> {
        &mut memo.has_user_method
    }
}

struct IsNativeMethod;
impl ProbeTable for IsNativeMethod {
    type Answer = bool;
    fn table(memo: &mut UserMethodProbeMemo) -> &mut FxHashMap<(Symbol, Symbol), bool> {
        &mut memo.is_native_method
    }
}

struct MethodOrAccessor;
impl ProbeTable for MethodOrAccessor {
    type Answer = Option<UserMethodOrAccessor>;
    fn table(
        memo: &mut UserMethodProbeMemo,
    ) -> &mut FxHashMap<(Symbol, Symbol), Option<UserMethodOrAccessor>> {
        &mut memo.method_or_accessor
    }
}

struct MethodCandidateLevels;
impl ProbeTable for MethodCandidateLevels {
    type Answer = Arc<[u32]>;
    fn table(memo: &mut UserMethodProbeMemo) -> &mut FxHashMap<(Symbol, Symbol), Arc<[u32]>> {
        &mut memo.method_candidate_levels
    }
}

struct PublicAccessorOwner;
impl ProbeTable for PublicAccessorOwner {
    type Answer = Option<Symbol>;
    fn table(memo: &mut UserMethodProbeMemo) -> &mut FxHashMap<(Symbol, Symbol), Option<Symbol>> {
        &mut memo.public_accessor_owner
    }
}

/// The memo key for a name a caller holds as `&str`. Every class and method a
/// probe can answer about was interned when it was declared, so a *lookup*
/// finds it -- and unlike `Symbol::intern` a lookup neither grows the intern
/// table nor counts against the per-call intern budgets
/// (`tests/named_call_intern_budget.rs`). A name that was never interned
/// declares nothing, so its caller skips the memo and answers directly.
// Cost: O(n), n = the name's length (one hash probe).
pub(crate) fn probe_key(name: &str) -> Option<Symbol> {
    Symbol::lookup(name)
}

impl Interpreter {
    /// Look `(class, method)` up in table `T`, computing and recording it with
    /// `compute` on a miss.
    // Cost: O(1) on a hit; a miss costs `compute`.
    fn probe_memo<T: ProbeTable>(
        &mut self,
        class: Symbol,
        method: Symbol,
        compute: impl FnOnce(&mut Self) -> T::Answer,
    ) -> T::Answer {
        let generation = self.registry_write_generation();
        let memo = &mut self.user_method_probe_memo;
        if memo.generation != generation {
            memo.generation = generation;
            memo.grammar_has_user_method.clear();
            memo.has_user_method.clear();
            memo.is_native_method.clear();
            memo.method_or_accessor.clear();
            memo.public_accessor_owner.clear();
            memo.method_candidate_levels.clear();
        } else if let Some(answer) = T::table(memo).get(&(class, method)) {
            return answer.clone();
        }
        let answer = compute(self);
        // Record it only if resolving the answer wrote nothing to the registry
        // (a role pun, an MRO computed and cached, say): an answer computed
        // across a registry write is not an answer for either generation.
        if self.registry_write_generation() == generation {
            T::table(&mut self.user_method_probe_memo).insert((class, method), answer.clone());
        }
        answer
    }

    /// [`Self::grammar_has_user_method_sym`], memoized per `(class, method)`
    /// for one registry write generation.
    // Cost: O(1) on a hit; a miss costs one MRO walk, O(d), d = MRO depth.
    pub(crate) fn grammar_has_user_method_memo(&mut self, class: Symbol, method: Symbol) -> bool {
        self.probe_memo::<GrammarHasUserMethod>(class, method, |this| {
            this.grammar_has_user_method_sym(class.as_str(), method)
        })
    }

    /// [`Self::has_user_method_uncached`], memoized per `(class, method)`.
    // Cost: O(1) on a hit; a miss costs one MRO walk, O(d), d = MRO depth.
    pub(crate) fn has_user_method_memo(&mut self, class: Symbol, method: Symbol) -> bool {
        self.probe_memo::<HasUserMethod>(class, method, |this| {
            this.has_user_method_uncached(class.as_str(), method)
        })
    }

    /// [`Self::is_native_method_uncached`], memoized per `(class, method)`.
    // Cost: O(1) on a hit; a miss costs one MRO walk, O(d), d = MRO depth.
    pub(crate) fn is_native_method_memo(&mut self, class: Symbol, method: Symbol) -> bool {
        self.probe_memo::<IsNativeMethod>(class, method, |this| {
            this.is_native_method_uncached(class.as_str(), method.as_str())
        })
    }

    /// [`Self::resolve_user_method_or_accessor_uncached`], memoized per
    /// `(class, method)`.
    // Cost: O(1) on a hit; a miss costs one MRO walk, O(d), d = MRO depth.
    pub(crate) fn resolve_user_method_or_accessor_memo(
        &mut self,
        class: Symbol,
        method: Symbol,
    ) -> Option<UserMethodOrAccessor> {
        self.probe_memo::<MethodOrAccessor>(class, method, |this| {
            this.resolve_user_method_or_accessor_uncached(class.as_str(), method)
        })
    }

    /// [`Self::first_public_accessor_owner_uncached`], memoized per
    /// `(class, method)`.
    // Cost: O(1) on a hit; a miss costs one MRO walk, O(d), d = MRO depth.
    pub(crate) fn first_public_accessor_owner(
        &mut self,
        class_name: &str,
        method: Symbol,
    ) -> Option<Symbol> {
        let Some(class) = probe_key(class_name) else {
            return self.first_public_accessor_owner_uncached(class_name, method);
        };
        self.probe_memo::<PublicAccessorOwner>(class, method, |this| {
            this.first_public_accessor_owner_uncached(class.as_str(), method)
        })
    }

    /// [`Self::method_candidate_levels_uncached`], memoized per
    /// `(class, method)`.
    // Cost: O(1) on a hit; a miss costs one MRO walk, O(d), d = MRO depth.
    pub(crate) fn method_candidate_levels(
        &mut self,
        class_name: &str,
        method_name: &str,
    ) -> Arc<[u32]> {
        let (Some(class), Some(method)) = (probe_key(class_name), probe_key(method_name)) else {
            return self.method_candidate_levels_uncached(class_name, method_name);
        };
        self.probe_memo::<MethodCandidateLevels>(class, method, |this| {
            this.method_candidate_levels_uncached(class.as_str(), method.as_str())
        })
    }

    /// The indices into `class_name`'s MRO of the levels that declare
    /// `method_name` (the levels `resolve_method_with_owner_impl` reads a
    /// candidate list from), in MRO order. Registry-pure; memoized.
    // Cost: O(d), d = MRO depth.
    fn method_candidate_levels_uncached(
        &mut self,
        class_name: &str,
        method_name: &str,
    ) -> std::sync::Arc<[u32]> {
        let mro = self.class_mro(class_name);
        let registry = self.registry();
        let class_registered = registry.classes.contains_key(class_name);
        mro.iter()
            .enumerate()
            .filter(|(_, cn)| {
                if cn.as_str() == class_name && !class_registered {
                    registry
                        .get_method_overloads_with_role_fallback(cn.as_str(), method_name)
                        .is_some()
                } else {
                    registry.method_overloads_present(**cn, method_name)
                }
            })
            .map(|(i, _)| i as u32)
            .collect()
    }

    /// The most-derived class on `class_name`'s MRO that declares a public
    /// accessor named `name`, whether or not an explicit method outranks it.
    // Cost: O(d), d = MRO depth.
    fn first_public_accessor_owner_uncached(
        &mut self,
        class_name: &str,
        name: crate::symbol::Symbol,
    ) -> Option<crate::symbol::Symbol> {
        self.class_mro(class_name)
            .iter()
            .find(|owner| {
                self.registry()
                    .accessor_is_public_sym(**owner, name)
                    .is_some_and(|is_public| is_public)
            })
            .copied()
    }
}
