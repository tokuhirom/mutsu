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
//! The value checks are user code, so they must run exactly once per call: a
//! hit that ran them and then fell back to the generic path would run them
//! again. So an entry records the light body of every candidate the program
//! can pick, and every candidate's light-bind admission for the call's
//! arguments is decided before the program runs. Only "no winner" falls back after the checks ran,
//! and the generic path then raises the dispatch failure. Only a program whose
//! checks are all inline WhateverCode `where`s
//! (`StageProgram::checks_are_inline_wheres`) is taken.

use super::*;
use crate::runtime::resolution_caches::{MultiLightWinner, MultiPlanLight};

impl Interpreter {
    /// The winner of a cached value-dependent multi call, with its compiled
    /// body, or `None` when the call is not one the cache can serve: no entry
    /// for `key`, a changed package search or proto/subset
    /// set, a wrapper or a test assertion, a candidate whose light bind does
    /// not admit these arguments, or no winner (the generic path then reports
    /// the dispatch failure, running the value checks again).
    ///
    /// Every candidate's admission is decided before the stage program runs,
    /// so a winner it picks is always taken.
    // Cost: O(c·p) admissions, c = candidates, p = their parameters, plus the
    // stage program's value checks.
    pub(super) fn multi_plan_light_winner(
        &mut self,
        code: &CompiledCode,
        name: &str,
        name_sym: Symbol,
        args: &[Value],
        key: &crate::runtime::FuncMultiResolveKey,
        compiled_fns: &CompiledFns,
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
        let argc = Self::positional_light_argc(args);
        let admitted = entry.winners.iter().all(|w| {
            compiled_fns
                .get(&w.key)
                .filter(|cf| cf.fingerprint == w.fingerprint)
                .is_some_and(|cf| {
                    let where_verified = w
                        .def
                        .param_defs
                        .iter()
                        .any(|pd| pd.where_constraint.is_some());
                    Self::is_positional_light_call_eligible_where(
                        cf,
                        name,
                        argc,
                        args,
                        Some(code),
                        where_verified,
                    ) && !Self::call_shares_container_into_scalar_param(cf, args)
                })
        });
        if !admitted {
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
    /// `winner` is one of its candidates, and the compiled body of every
    /// candidate the program can pick is found.
    // Cost: O(a + c) for a winner already recorded, a = arguments,
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
        let pkg_sym = self.current_package_sym();
        let generation = self.caches.fn_resolve_gen;
        // A family refused once is refused for every key: its calls skip the
        // key derivation below (an allocation per call on
        // `bench-multi-dispatch`'s subset family).
        if self
            .caches
            .multi_plan_light_refused
            .get(generation, &(pkg_sym, name_sym), || {
                self.dispatch.fn_family_gens.of_name(name_sym)
            })
            .is_some()
        {
            return;
        }
        let Some(keys) = self.multi_arg_type_keys(args) else {
            return;
        };
        let key = (pkg_sym, name_sym, keys);
        if self
            .caches
            .multi_plan_light_cache
            .get(generation, &key, || {
                self.dispatch.fn_family_gens.of_name(name_sym)
            })
            .is_some()
        {
            return;
        }
        let plan = if self.is_unit_scoped_routine_name(name) {
            None
        } else {
            self.light_bare_multi_plan(name, args, &key.2)
        };
        let Some((plan_key, plan)) = plan else {
            self.caches.multi_plan_light_refused.insert(
                generation,
                self.caches.fn_resolve_gen,
                || self.dispatch.fn_family_gens.of_name(name_sym),
                (pkg_sym, name_sym),
                (),
            );
            return;
        };
        let candidates = Self::plan_program_candidates(&plan);
        if !candidates.iter().any(|c| Arc::ptr_eq(c, winner)) {
            return;
        }
        // The winner's body is the one this call is about to run; every other
        // candidate's is found the way `find_compiled_function_inner` finds a
        // multi winner's, by its body fingerprint, among the family's keys.
        let base = crate::runtime::dispatch_resolve::function_key_base_name(name);
        let mut winners = Vec::with_capacity(candidates.len());
        for candidate in &candidates {
            let fingerprint = candidate.body_fingerprint();
            let mut found = compiled_fns.iter().filter(|(k, f)| {
                if Arc::ptr_eq(candidate, winner) {
                    std::ptr::eq(Arc::as_ptr(f), cf)
                } else {
                    f.fingerprint == fingerprint
                        && crate::runtime::dispatch_resolve::function_key_base_name(k.as_str())
                            == base
                }
            });
            let (Some((body_key, body)), None) = (found.next(), found.next()) else {
                // No body yet, or two that cannot be told apart: the family
                // stays on the generic path.
                return;
            };
            winners.push(MultiLightWinner {
                def: Arc::clone(candidate),
                key: *body_key,
                fingerprint: body.fingerprint,
            });
        }
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
