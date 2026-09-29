//! Per-type dispatch programs for value-dependent multi calls
//! ([#10107](https://github.com/tokuhirom/mutsu/issues/10107)).
//!
//! A [`BareMultiPlan`](super::multi_dispatch_plan) stage already fixes, per
//! argument-type key, which candidates are gathered and in what narrowness
//! order. Binding each of them against the arguments still re-derived, on
//! every call, everything that key already decides: arity, every nominal type
//! check, native-literal admission. Only the checks that read an argument's
//! *value* — a `subset` constraint and a `where` clause — can differ between
//! two calls with the same key.
//!
//! A [`StageProgram`] is that split made explicit, the way Rakudo's
//! dispatcher specializes a multi on the argument types and keeps only the
//! value-dependent guards: built once per stage from the first call with the
//! key, it holds each candidate's precomputed nominal verdict and the list of
//! value checks left to run. [`Interpreter::run_stage_program`] runs those
//! checks and hands the matches to the same settling code the general walk
//! uses (`settle_ranked_matches`), so ties, `is default` and ambiguity are
//! decided exactly as before.
//!
//! A stage gets a program only when every candidate in it has a plain
//! positional signature (`args_match_simple_positional`'s shape) and a rank
//! that does not read argument values; any other stage keeps the general walk.

use super::dispatch_candidates::CandidateRankKey;
use super::multi_dispatch_plan::PlanEntry;
use super::*;

/// A value-dependent check left for call time, against argument `arg`.
enum ValueCheck {
    /// The argument must match a type whose match reads its value (a
    /// `subset`, `UInt`); run through `type_matches_value`.
    Type { arg: usize, constraint: String },
    /// Parameter `param`'s one-argument WhateverCode `where`, precompiled
    /// inline (ADR-0133), run with `$_` bound to the argument.
    Where { arg: usize, param: usize },
}

struct ProgramStep {
    key: CandidateRankKey,
    fingerprint: u64,
    def: Arc<FunctionDef>,
    /// Every check that the argument-type key decides, answered once.
    nominal_ok: bool,
    checks: Vec<ValueCheck>,
}

/// One plan stage, specialized to its argument-type key. See the module docs.
pub(crate) struct StageProgram {
    /// Unique by fingerprint, in narrowness order.
    steps: Vec<ProgramStep>,
}

impl Interpreter {
    /// Build the program for `stage` from a call whose arguments carry the
    /// stage's type key, or `None` when some candidate needs the general walk.
    // Cost: O(c·p) type checks, c = candidates, p = parameters; once per key.
    pub(super) fn build_stage_program(
        &mut self,
        args: &[Value],
        stage: &[PlanEntry],
    ) -> Option<StageProgram> {
        if stage.is_empty()
            || args
                .iter()
                .any(|arg| arg.unwrap_varref().is_string_pair_value())
        {
            return None;
        }
        let mut steps: Vec<ProgramStep> = Vec::with_capacity(stage.len());
        for entry in stage {
            if entry.reads_value {
                return None;
            }
            let fingerprint = entry.def.body_fingerprint();
            if steps.iter().any(|s| s.fingerprint == fingerprint) {
                continue;
            }
            let def = &entry.def;
            // A placeholder-signature block (`params` of `^a` with no
            // `param_defs`) is matched by arity alone in the general walk.
            if def.param_defs.len() != args.len()
                || (def.param_defs.is_empty() && !def.params.is_empty())
                || !def.param_defs.iter().all(Self::is_simple_positional_param)
                || !Self::simple_where_reads_no_parameter(&def.param_defs)
            {
                return None;
            }
            let (nominal_ok, checks) = self.with_candidate_package(Some(def.package), |this| {
                this.split_candidate_checks(args, &def.param_defs)
            })?;
            steps.push(ProgramStep {
                key: entry.key,
                fingerprint,
                def: def.clone(),
                nominal_ok,
                checks,
            });
        }
        steps.sort_by(|a, b| Self::candidate_rank_cmp(a.key, b.key));
        Some(StageProgram { steps })
    }

    /// Answer every key-decided check of one candidate now, and list the
    /// value checks left. `None` when a constraint still needs resolving
    /// (a type capture, a package alias), which is the general walk's job.
    fn split_candidate_checks(
        &mut self,
        args: &[Value],
        param_defs: &[ParamDef],
    ) -> Option<(bool, Vec<ValueCheck>)> {
        let mut nominal_ok = true;
        let mut checks = Vec::new();
        for (idx, (pd, raw)) in param_defs.iter().zip(args).enumerate() {
            let arg = crate::runtime::types::unwrap_varref_value_for_dispatch(raw);
            match pd.type_constraint.as_deref() {
                Some(tc) => {
                    if self.try_resolved_type_capture_name(tc).is_some() {
                        return None;
                    }
                    if self.constraint_is_subset(tc) {
                        checks.push(ValueCheck::Type {
                            arg: idx,
                            constraint: tc.to_string(),
                        });
                    } else if !self.native_dispatch_arg_matches(tc, args, Some(idx), &arg)
                        || !self.type_matches_value(tc, &arg)
                    {
                        nominal_ok = false;
                    }
                }
                None => {
                    if !self.type_matches_value("Any", &arg) {
                        nominal_ok = false;
                    }
                }
            }
            if pd.where_constraint.is_some() {
                checks.push(ValueCheck::Where {
                    arg: idx,
                    param: idx,
                });
            }
        }
        Some((nominal_ok, checks))
    }

    /// Run `program` against this call's arguments: the per-call half of a
    /// stage, equivalent to `choose_from_plan_stage`.
    // Cost: O(c) plus the value checks, c = candidates up to the best rank.
    pub(super) fn run_stage_program(
        &mut self,
        name: &str,
        args: &[Value],
        program: &StageProgram,
        rejected: &mut std::collections::HashSet<u64>,
    ) -> Option<Arc<FunctionDef>> {
        let mut matches: Vec<Arc<FunctionDef>> = Vec::new();
        let mut best_key: Option<CandidateRankKey> = None;
        let mut threw: Option<(Arc<FunctionDef>, RuntimeError)> = None;
        let outer_where_exception = self.pending_where_exception.take();
        for step in &program.steps {
            if rejected.contains(&step.fingerprint) {
                continue;
            }
            if let Some(best) = best_key
                && Self::candidate_rank_cmp(
                    Self::rank_key_ignoring_decl_order(step.key),
                    Self::rank_key_ignoring_decl_order(best),
                ) == std::cmp::Ordering::Greater
            {
                break;
            }
            let ok = step.nominal_ok
                && self.with_candidate_package(Some(step.def.package), |this| {
                    this.run_value_checks(args, &step.def.param_defs, &step.checks)
                });
            if let Some(e) = self.take_where_exception() {
                if threw.is_none() {
                    threw = Some((step.def.clone(), e));
                }
                continue;
            }
            if !ok {
                rejected.insert(step.fingerprint);
                continue;
            }
            best_key.get_or_insert(step.key);
            matches.push(step.def.clone());
        }
        if matches.is_empty() && threw.is_none() {
            self.pending_where_exception = outer_where_exception;
            return None;
        }
        self.settle_ranked_matches(name, args, matches, threw, outer_where_exception)
    }

    fn run_value_checks(
        &mut self,
        args: &[Value],
        param_defs: &[ParamDef],
        checks: &[ValueCheck],
    ) -> bool {
        for check in checks {
            match check {
                ValueCheck::Type { arg, constraint } => {
                    let value =
                        crate::runtime::types::unwrap_varref_value_for_dispatch(&args[*arg]);
                    if !self.type_matches_value(constraint, &value) {
                        return false;
                    }
                }
                ValueCheck::Where { arg, param } => {
                    let value =
                        crate::runtime::types::unwrap_varref_value_for_dispatch(&args[*arg]);
                    if !self.simple_where_holds(&param_defs[*param], value) {
                        return false;
                    }
                }
            }
        }
        true
    }
}
