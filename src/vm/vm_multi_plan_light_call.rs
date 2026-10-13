//! A value-dependent multi family on the light call path (ADR-12529 §2.4).
//!
//! `multi_light_cache` serves only a family whose winner is a pure function of
//! the argument types. One `where` clause withholds it from the whole family,
//! and every call then took the generic path: the resolution prologue (name
//! interning, the package search, the plan-key hash) around the family's
//! dispatch plan, then the compiled-body probes — about 18k instructions a
//! call against 10.8k for a type-only family, although the `where` itself
//! (`* > 0`) costs a few hundred.
//!
//! The plan's stage program already holds the type-keyed half of the dispatch
//! (#9967, #10107); only its value checks differ per call. So
//! `multi_plan_light_cache`, keyed like `multi_light_cache`, keeps the family's
//! plan and the compiled body of each winner the generic path has light-bound,
//! and a call with the same argument types runs the program and goes to the
//! winner's light bind.
//!
//! The program runs before the winner's own light-bind admissions are known,
//! so a winner the bind does not admit for these arguments falls back to the
//! generic path, which runs the value checks again. Only a program whose
//! checks are all inline WhateverCode `where`s
//! (`StageProgram::checks_are_inline_wheres`) is taken: a `subset` predicate
//! may be any user block.

use super::*;
use crate::runtime::resolution_caches::{MultiLightWinner, MultiPlanLight};

impl Interpreter {
    /// The winner of a cached value-dependent multi call, with its compiled
    /// body, or `None` when the call is not one the cache can serve: no entry
    /// for `key`, a changed package search or proto/subset set, a wrapper or a
    /// test assertion, a winner whose body the light path has not run yet, or
    /// no winner (the generic path reports the dispatch failure).
    // Cost: O(1) cache probe plus the stage program's value checks.
    pub(super) fn multi_plan_light_winner(
        &mut self,
        name: &str,
        name_sym: Symbol,
        args: &[Value],
        key: &crate::runtime::FuncMultiResolveKey,
    ) -> Option<MultiLightWinner> {
        if self.caches.multi_plan_light_cache.is_empty() {
            return None;
        }
        let entry =
            self.caches
                .multi_plan_light_cache
                .get(self.caches.fn_resolve_gen, key, || {
                    self.dispatch.fn_family_gens.of_name(name_sym)
                })?;
        if !self.light_plan_key_current(&entry.plan_key)
            || self.dispatch.empty_sig_proto_names.contains(&name_sym)
            || loan_env!(self, routine_is_test_assertion_by_name(name, args))
            || self.wrap_sub_id_for_name(name).is_some()
            || self.light_call_blocked_by_mainline_capture(name)
        {
            return None;
        }
        let Ok(Some(winner)) = self.run_light_bare_multi_plan(name, args, &entry.plan) else {
            return None;
        };
        entry
            .winners
            .iter()
            .find(|w| Arc::ptr_eq(&w.def, &winner))
            .cloned()
    }

    /// Record a value-dependent multi call the generic path is about to run
    /// through the positional light bind, for [`Self::multi_plan_light_winner`]:
    /// only when the family's keyed plan has an inline-checks program and
    /// `winner` is one of its candidates.
    // Cost: O(a + c) for a family already recorded, a = arguments,
    // c = candidates; plus O(f) to find the body's key, f = `compiled_fns`
    // entries, once per winner.
    pub(super) fn note_multi_plan_light_call(
        &mut self,
        name: &str,
        name_sym: Symbol,
        args: &[Value],
        winner: &Arc<crate::ast::FunctionDef>,
        cf: &CompiledFunction,
        compiled_fns: &CompiledFns,
    ) {
        if self.is_unit_scoped_routine_name(name) {
            return;
        }
        let Some(keys) = self.multi_arg_type_keys(args) else {
            return;
        };
        let Some((plan_key, plan)) = self.light_bare_multi_plan(name, args, &keys) else {
            return;
        };
        if !Self::plan_has_candidate(&plan, winner) {
            return;
        }
        let key = (self.current_package_sym(), name_sym, keys);
        let generation = self.caches.fn_resolve_gen;
        let previous = self
            .caches
            .multi_plan_light_cache
            .get(generation, &key, || {
                self.dispatch.fn_family_gens.of_name(name_sym)
            });
        let mut winners = match &previous {
            Some(entry) if Arc::ptr_eq(&entry.plan, &plan) => entry.winners.clone(),
            _ => Vec::new(),
        };
        if winners.iter().any(|w| Arc::ptr_eq(&w.def, winner)) {
            return;
        }
        let Some(body_key) = compiled_fns
            .iter()
            .find(|(_, f)| std::ptr::eq(Arc::as_ptr(f), cf))
            .map(|(k, _)| *k)
        else {
            return;
        };
        winners.push(MultiLightWinner {
            def: Arc::clone(winner),
            key: body_key,
            fingerprint: cf.fingerprint,
        });
        self.caches.multi_plan_light_cache.insert(
            generation,
            self.caches.fn_resolve_gen,
            || self.dispatch.fn_family_gens.of_name(name_sym),
            key,
            Arc::new(MultiPlanLight {
                plan_key,
                plan,
                winners,
            }),
        );
    }
}
