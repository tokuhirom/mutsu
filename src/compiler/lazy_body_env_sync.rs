//! The bounded env-sync set of a named sub's lazily-registered body (#10960).
//!
//! A named sub is installed by a `RegisterDecl` op and has no runtime
//! closure-creation op, so the declaring frame's `compute_needs_env_sync`
//! cannot see which of its lexicals the body reads by name. It used to fold
//! EVERY local of such a frame into `needs_env_sync`, which made each store in
//! a top-level loop pay an env mirror write as soon as the script declared
//! any sub. The body is compiled right at its declaration, though, so its
//! free-variable set is known there: this module resolves it against the
//! declaring frame's slots and records it in
//! [`crate::opcode::CompiledCode::lazy_body_env_sync_slots`], marking the plan
//! bounded so the frame-wide fold skips it.
//!
//! The bar is the one the nested-closure fold in `compute_needs_env_sync`
//! already meets: every name the body (or anything nested in it) can read by
//! name from this frame's env must keep its mirror. A body that reaches names
//! no scan can bound stays unbounded and keeps the conservative fold.

use super::Compiler;
use crate::opcode::{CompiledCode, OpCode};
use crate::symbol::Symbol;
use crate::value::{Value, ValueView};

impl Compiler {
    /// Record the env-sync slots of a named sub's compiled bodies `keys` and
    /// mark sub-declaration plans `plan_idxs` bounded. Leaves the plans
    /// unbounded (the frame keeps its every-local fold) when a body failed to
    /// compile (`expected` bodies but fewer `keys`) or reads names by a
    /// mechanism no op scan can bound.
    // Cost: O(b), b = total ops and constants of the sub's compiled bodies,
    // nested closures included.
    pub(super) fn note_sub_decl_env_sync(
        &mut self,
        plan_idxs: &[u32],
        keys: &[Symbol],
        expected: usize,
    ) {
        if keys.len() != expected {
            return;
        }
        let mut names: Vec<Symbol> = Vec::new();
        for key in keys {
            let Some(cf) = self.compiled_functions.get(key) else {
                return;
            };
            if !lazy_body_reads_bounded(&cf.code) {
                return;
            }
            collect_by_name_reads(&cf.code, &mut names);
        }
        for sym in names {
            let slot = sym.with_str(|s| {
                self.local_map.get(s).copied().or_else(|| {
                    // `@$x` / `%$x` record `@x` / `%x`, but the lexical is the
                    // scalar `x` (see the closure fold this mirrors).
                    s.strip_prefix(['@', '%', '&'])
                        .and_then(|bare| self.local_map.get(bare).copied())
                })
            });
            if let Some(slot) = slot
                && !self.code.lazy_body_env_sync_slots.contains(&slot)
            {
                self.code.lazy_body_env_sync_slots.push(slot);
            }
        }
        for &idx in plan_idxs {
            if !self.code.bounded_lazy_sub_plans.contains(&idx) {
                self.code.bounded_lazy_sub_plans.push(idx);
            }
        }
    }
}

/// Whether every by-name read of `code` (and of each closure nested in it)
/// is visible to [`collect_by_name_reads`]. An interpolating or indirect
/// regex, a dynamic substitution replacement, a deferred phaser, and a
/// nested declaration that is not itself a bounded sub all resolve names the
/// op scan cannot enumerate.
// Cost: O(b), b = total ops and constants of `code` and its nested closures.
fn lazy_body_reads_bounded(code: &CompiledCode) -> bool {
    let own = code.ops.iter().all(|op| match op {
        OpCode::RegisterDecl(idx) => code.bounded_lazy_sub_plans.contains(idx),
        OpCode::PhaserEnd { .. } | OpCode::CheckPhaser { .. } => false,
        _ => true,
    }) && !code.holds_interpolating_regex()
        && !code.holds_dynamic_substitution()
        && !code.holds_indirect_regex_lookup();
    own && code
        .closure_compiled_codes
        .iter()
        .all(|c| lazy_body_reads_bounded(c))
}

/// Every name `code` may resolve by name in an enclosing frame's env: its
/// free reads and writes (which already include its nested closures', nested
/// routines' and `gather`/`whenever` bodies'), the rw-arg-sink targets, and —
/// at any closure depth — the scalars it mutates in place and the bare
/// callee names (a call `e()` colliding with an outer `my $e` reads `env[e]`).
// Cost: O(b), b = total ops of `code` and its nested closures.
fn collect_by_name_reads(code: &CompiledCode, out: &mut Vec<Symbol>) {
    let mut push = |sym: Symbol| {
        if !out.contains(&sym) {
            out.push(sym);
        }
    };
    for sym in code
        .free_var_syms
        .iter()
        .chain(&code.free_var_writes)
        .chain(&code.free_var_container_writes)
        .chain(&code.rw_arg_env_sync_syms)
    {
        push(*sym);
    }
    for op in &code.ops {
        let idx = code
            .op_container_mutate_const_idx(op)
            .or_else(|| CompiledCode::op_callee_name_const_idx(op));
        if let Some(idx) = idx
            && let Some(ValueView::Str(name)) = code.constants.get(idx as usize).map(Value::view)
            && !code.locals.iter().any(|l| l.as_str() == name.as_str())
        {
            push(Symbol::intern(name.as_str()));
        }
    }
    for nested in &code.closure_compiled_codes {
        collect_by_name_reads(nested, out);
    }
}
