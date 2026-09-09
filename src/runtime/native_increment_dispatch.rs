//! The native `++`/`--` implementations as multi-dispatch candidates.
//!
//! Rakudo's `&prefix:<++>` is a `multi` with nine core candidates (`Mu:D`,
//! `Mu:U`, `Int:D`, `int`, `uint`, `Bool`, `Num:D`, `Num:U`, `num`); the other
//! three increment operators have the same shape. A user `multi prefix:<++>`
//! therefore joins a candidate *set* rather than replacing the operator, and
//! whether it runs is decided by ordinary narrowness ranking against those core
//! candidates.
//!
//! mutsu implements the increment natively, so there is no `FunctionDef` for a
//! user candidate to lose a narrowness comparison against. This module supplies
//! the missing half: it models the core candidate set as the type constraint
//! each argument would bind to, ranks that constraint against the winning user
//! candidate using the very metrics multi dispatch already ranks by
//! ([`Interpreter::candidate_specificity_rank_for_args`] and
//! [`Interpreter::candidate_type_distance`]), and — when the core candidate
//! wins — performs the increment through the container reference the call site
//! already wrapped the operand in.
//!
//! Only the *typed* core candidates can beat an untyped user parameter. An
//! untyped parameter is `Any`, which out-narrows `Mu:D`/`Mu:U`, so a `Rat`, a
//! `Str`, a user class instance or an undefined `Any` still reaches the user's
//! candidate — exactly as in rakudo.

use super::*;
use crate::runtime::dispatch_candidates::UNRELATED_DISTANCE;

/// Which of the four increment operators a call site names.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum IncrementOp {
    PreInc,
    PreDec,
    PostInc,
    PostDec,
}

impl IncrementOp {
    /// The operator whose core candidates a routine name stands for, or `None`
    /// for any other routine.
    pub(crate) fn from_routine_name(name: &str) -> Option<Self> {
        match name {
            "prefix:<++>" => Some(Self::PreInc),
            "prefix:<-->" => Some(Self::PreDec),
            "postfix:<++>" => Some(Self::PostInc),
            "postfix:<-->" => Some(Self::PostDec),
            _ => None,
        }
    }

    fn increments(self) -> bool {
        matches!(self, Self::PreInc | Self::PostInc)
    }

    /// `++$x` evaluates to the new value, `$x++` to the old one.
    fn yields_old_value(self) -> bool {
        matches!(self, Self::PostInc | Self::PostDec)
    }
}

impl Interpreter {
    /// The narrowest *core* candidate that accepts `value`: either one of the
    /// typed candidates (`Int:D` / `Bool` / `Num:D`+`Num:U`, together with the
    /// native `int`/`uint`/`num` twins, which are exactly as narrow) with its
    /// distance from the argument's type, or `Mu` — the `Mu:D`/`Mu:U` pair that
    /// catches everything else.
    ///
    /// `Int` is offered only for a definite argument: rakudo has an `Int:D`
    /// candidate but no `Int:U` one, so `my Int $i; ++$i` falls through to
    /// `Mu:U` and reaches a user candidate. `Bool` and `Num` are offered at
    /// either definiteness because the core set covers both.
    fn core_increment_candidate(&self, value: &Value) -> (&'static str, usize) {
        let definite = crate::runtime::types::value_is_defined(value);
        // An allomorph / role-mixed value (`<42>`, `1 but Foo`) is of its base
        // type for dispatch, and `type_hierarchy_distance` does not look
        // through the wrapper -- rakudo runs the core `Int:D` candidate for
        // `++<42>`, not a user `Any` one.
        let probe = match value.view() {
            ValueView::Mixin(inner, _) => inner.as_ref().clone(),
            _ => value.clone(),
        };
        let mut best: Option<(&'static str, usize)> = None;
        for constraint in ["Int", "Bool", "Num"] {
            if constraint == "Int" && !definite {
                continue;
            }
            let distance = self.type_hierarchy_distance(constraint, &probe);
            if distance >= UNRELATED_DISTANCE {
                continue;
            }
            if best.is_none_or(|(_, best_distance)| distance < best_distance) {
                best = Some((constraint, distance));
            }
        }
        best.unwrap_or(("Mu", UNRELATED_DISTANCE))
    }

    /// The type constraint the winning user candidate dispatches its single
    /// positional parameter on. `None` for an unconstrained parameter, which
    /// raku reads as `Any`.
    fn user_increment_constraint(def: &FunctionDef) -> Option<String> {
        def.param_defs
            .iter()
            .find(|p| !p.named)
            .and_then(|p| p.type_constraint.clone())
    }

    /// Does the core candidate set out-rank the user candidate for this call?
    ///
    /// `None` means the question does not arise: not an increment operator, or
    /// the user declared a plain `sub`, which is a lexical shadow rather than a
    /// candidate and replaces the operator outright — as in rakudo.
    ///
    /// Ties go to the core candidate: rakudo runs the builtin for a user
    /// `multi prefix:<++>(Int:D $a)`, with or without `is default`, and a user
    /// `multi prefix:<++>(Mu $a)` likewise loses to the core `Mu:D`.
    pub(crate) fn core_increment_candidate_wins(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Option<bool> {
        IncrementOp::from_routine_name(name)?;
        if args.len() != 1 {
            return None;
        }
        if !self.has_multi_function_cached(name) {
            return None;
        }
        let operand = Self::increment_operand(&args[0]);
        let (core_constraint, core_distance) = self.core_increment_candidate(&operand);
        // Rank against the user candidate multi dispatch would otherwise run.
        // With no matching user candidate there is nothing to compare and the
        // core implementation runs by default. `args` is passed through as the
        // call site built it (the operand still wrapped in its `VarRef`) so an
        // `is rw` user parameter still binds — both resolution and the ranking
        // metrics unwrap it themselves.
        let Some(def) = self.resolve_function_with_types(name, args) else {
            return Some(true);
        };
        if core_constraint == "Mu" {
            // `Mu` is the root of the type hierarchy, so every other parameter
            // type — an unconstrained parameter included, since that is `Any` —
            // is strictly narrower and takes the call. Only a user candidate
            // that is itself constrained to `Mu` ties, and a tie goes to core.
            return Some(
                Self::user_increment_constraint(&def)
                    .as_deref()
                    .map(Self::constraint_base_name)
                    .is_some_and(|base| base == "Mu"),
            );
        }
        // A typed core candidate. Rank against the user's the way multi dispatch
        // does, but with *nominal type* narrowness as the primary key: a
        // refinement (subset / `where` / literal) only breaks a tie between
        // equally narrow nominal types, so an untyped `($a where * > 0)` still
        // loses to the core `Int:D` for an `Int` argument. The core candidate
        // is a plain nominal type, so it carries one typed positional and no
        // refinement of any kind.
        let (literal, where_c, subset, typed, subsig, _traits) =
            self.candidate_specificity_rank_for_args(&def, args);
        let user_key = (typed, literal, where_c, subset, subsig);
        let core_key = (1usize, 0usize, 0usize, 0usize, 0usize);
        if user_key != core_key {
            return Some(core_key > user_key);
        }
        let user_distance = self.candidate_type_distance(args, &def);
        Some(core_distance <= user_distance)
    }

    /// The value an increment operand carries, looking through the `VarRef`
    /// wrapper the call site adds so the callee can write back.
    fn increment_operand(arg: &Value) -> Value {
        match arg.as_varref() {
            Some((_, inner, _)) => inner.clone(),
            None => arg.clone(),
        }
    }

    /// Run the core `++`/`--` implementation for a call site that resolved to
    /// it, storing the new value back through the operand's container reference
    /// the way rakudo's `is rw` core candidates do.
    ///
    /// The caller is responsible for draining `pending_rw_writeback_sources`
    /// (`apply_pending_rw_writeback`) so the write reaches the caller's local
    /// slot as well as `env`.
    pub(crate) fn run_core_increment(
        &mut self,
        op: IncrementOp,
        arg: &Value,
        code: Option<&crate::opcode::CompiledCode>,
    ) -> Result<Value, RuntimeError> {
        let (source, raw, index) = match arg.as_varref() {
            Some((name, inner, index)) => (
                Some(name.resolve().to_string()),
                inner.clone(),
                index.map(|i| i as usize),
            ),
            None => (None, arg.clone(), None),
        };
        // An undefined operand starts from its type's zero (`my Num $n; $n++`
        // is `0e0`, `my Bool $b; $b++` is `False`), matching the core
        // candidates' declared return values.
        let old = match source.as_deref() {
            Some(name) => self.normalize_incdec_source_with_type(name, raw),
            None => Self::normalize_incdec_source(raw),
        };
        let new = if op.increments() {
            self.increment_value_smart(&old)?
        } else {
            self.decrement_value_smart(&old)?
        };
        let new = match source.as_deref() {
            Some(name) => self.wrap_native_int_arithmetic_result(name, new),
            None => new,
        };
        if let Some(name) = source {
            self.check_incdec_type_constraint(&name, &new)?;
            match code {
                Some(code) => self.store_core_increment_result(code, &name, index, &new)?,
                // The re-entrant path has no frame to reach the richer store
                // through; the `VarRef` name/index write is what a user `is rw`
                // candidate would do from there too.
                None => self.assign_varref_target(&name, index, new.clone())?,
            }
            self.pending_rw_writeback_sources.push(name);
        }
        Ok(if op.yields_old_value() { old } else { new })
    }
}
