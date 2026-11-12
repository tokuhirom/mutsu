//! Multi-method `where` clauses are checked lazily, in candidate order.
//!
//! Rakudo's dispatcher sorts a multi's candidates by nominal narrowness and
//! walks them in that order; a candidate that carries a `where` clause (a
//! "bind check") is trial-bound only when the walk reaches it, and the first
//! one that binds wins. So a `where` clause of a candidate ranked below the
//! winner never runs:
//!
//! ```raku
//! class P {
//!     multi method m(Int $x where { say 'p'; True }) { 'p' }
//!     multi method m(Int $x where { say 'q'; True }) { 'q' }
//! }
//! P.m(1);   # rakudo prints only "p"
//! ```
//!
//! mutsu matched every candidate in full — `where` clauses included — and
//! then ranked the ones that bound. Besides running user code rakudo does
//! not run, that made a `where` clause the dominant cost of an
//! optimizer-style multi (Red's `Red::AST::Optimizer::AND`, whose
//! `where compare($left, $right)` clauses each compare two ASTs).
//!
//! Here a `multi` candidate with a `where` clause is first matched on the
//! rest of its signature (its *nominal* part, [`Interpreter::nominal_param_defs`]),
//! and ranked as if it bound. When the ranking picks such a candidate, its
//! full signature is checked then; if it does not bind it becomes a rival
//! (exactly as a failed full match did before) and the ranking runs again
//! over the rest.

use super::*;
use std::sync::Arc;

impl Interpreter {
    /// Whether matching `def` evaluates a `where` clause of its own
    /// parameters (the invocant's included).
    // Cost: O(p), p = parameters.
    pub(super) fn method_def_has_where(def: &MethodDef) -> bool {
        def.param_defs.iter().any(|p| p.where_constraint.is_some())
    }

    /// `def`'s parameter list with every `where` clause (and literal) dropped, memoized by
    /// the list's address (the cache entry keeps the original alive, so the
    /// address is not reused while the entry exists).
    // Cost: O(1) amortized; O(p) to build on a miss, p = parameters.
    fn nominal_param_defs(&mut self, def: &MethodDef) -> Arc<Vec<ParamDef>> {
        let key = Arc::as_ptr(&def.param_defs) as usize;
        if let Some((_, stripped)) = self.caches.nominal_param_defs.get(&key) {
            return stripped.clone();
        }
        let stripped: Arc<Vec<ParamDef>> = Arc::new(
            def.param_defs
                .iter()
                .map(|p| {
                    // A literal parameter is a `where` constraint too
                    // (`$ where 1`): rakudo checks it during the same trial
                    // bind, in parameter order, so it is left to the full
                    // match as well.
                    let mut p = p.clone();
                    p.where_constraint = None;
                    p.literal_value = None;
                    p
                })
                .collect(),
        );
        self.caches
            .nominal_param_defs
            .insert(key, (def.param_defs.clone(), stripped.clone()));
        stripped
    }

    /// Whether `args` match `def`'s signature with its `where` clauses left
    /// out: the part of a bind check rakudo decides while sorting.
    // Cost: as `method_args_match_for_invocant`, minus the `where` clauses.
    pub(super) fn method_args_match_nominal(
        &mut self,
        class_name: &str,
        def: &MethodDef,
        arg_values: &[Value],
        role_bindings: Option<&rustc_hash::FxHashMap<String, Value>>,
        invocant: Option<&Value>,
    ) -> bool {
        let mut nominal = def.clone();
        nominal.param_defs = self.nominal_param_defs(def);
        self.method_args_match_for_invocant(class_name, &nominal, arg_values, role_bindings, invocant)
    }

    /// [`Self::pick_method_winner`] over `all_matches`, where the entries
    /// flagged in `unverified` matched only nominally: a winner among those
    /// is checked in full before it is returned, and one that does not bind
    /// is moved to `rivals` and the ranking repeated.
    // Cost: O(u) rankings (each `pick_method_winner`'s cost), u = the
    // unverified candidates that rank first and fail their `where`; one full
    // match per unverified candidate the ranking reaches.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn pick_method_winner_lazy_where(
        &mut self,
        mro: &[Symbol],
        class_name: &str,
        arg_values: &[Value],
        invocant: Option<&Value>,
        role_bindings: Option<&rustc_hash::FxHashMap<String, Value>>,
        mut all_matches: Vec<(Symbol, MethodDef)>,
        mut unverified: Vec<bool>,
        mut rivals: Vec<(Symbol, Rival)>,
    ) -> Option<(Symbol, MethodDef)> {
        debug_assert_eq!(all_matches.len(), unverified.len());
        if !unverified.contains(&true) {
            return self.pick_method_winner(mro, arg_values, invocant, all_matches, rivals);
        }
        let saved_ambiguous = self.dispatch.dispatch_ambiguous;
        loop {
            if all_matches.is_empty() {
                return None;
            }
            self.dispatch.dispatch_ambiguous = saved_ambiguous;
            let (owner, def) = self.pick_method_winner(
                mro,
                arg_values,
                invocant,
                all_matches.clone(),
                rivals.clone(),
            )?;
            let Some(idx) = all_matches.iter().position(|(o, d)| {
                *o == owner
                    && Arc::ptr_eq(&d.param_defs, &def.param_defs)
                    && Arc::ptr_eq(&d.body, &def.body)
            }) else {
                return Some((owner, def));
            };
            if !unverified[idx]
                || self.method_args_match_for_invocant(
                    class_name,
                    &def,
                    arg_values,
                    role_bindings,
                    invocant,
                )
            {
                return Some((owner, def));
            }
            let (owner, def) = all_matches.remove(idx);
            unverified.remove(idx);
            rivals.push((owner, Rival::owned(def)));
        }
    }

    /// Check every nominally matched entry of `all_matches` in full now,
    /// moving the ones that do not bind to `rivals`. A non-`multi` MRO level
    /// decides by whether anything bound so far (a plain method on an
    /// ancestor wins only when no more-derived multi bound), so the deferred
    /// `where` clauses are settled before such a level is looked at.
    // Cost: one full match per unverified entry.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn verify_lazy_where_matches(
        &mut self,
        class_name: &str,
        arg_values: &[Value],
        invocant: Option<&Value>,
        role_bindings: Option<&rustc_hash::FxHashMap<String, Value>>,
        all_matches: &mut Vec<(Symbol, MethodDef)>,
        unverified: &mut Vec<bool>,
        rivals: &mut Vec<(Symbol, Rival)>,
    ) {
        let mut i = 0;
        while i < all_matches.len() {
            if unverified[i] {
                let bound = self.method_args_match_for_invocant(
                    class_name,
                    &all_matches[i].1,
                    arg_values,
                    role_bindings,
                    invocant,
                );
                if !bound {
                    let (owner, def) = all_matches.remove(i);
                    unverified.remove(i);
                    rivals.push((owner, Rival::owned(def)));
                    continue;
                }
                unverified[i] = false;
            }
            i += 1;
        }
    }
}
