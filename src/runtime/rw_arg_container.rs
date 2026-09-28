//! ADR-0067, the argument producer: does a named routine bind its Nth
//! positional argument to the *caller's container*?
//!
//! This is the gate behind [`crate::opcode::OpCode::MarkRwArgRefContext`]. The
//! consuming side of argument position was never broken — the binder already
//! accepts a bare `ContainerRef` argument as a writable lvalue
//! (`binding_signature.rs`) — what was missing is a producer: an attribute
//! accessor read compiles to a value copy in argument position, so
//! `sub g($y is rw) { $y = 9 }; g($c.v)` died with "expects a writable
//! container" while raku wrote `9`, and the `is raw` twin
//! (`sub f(\x) is raw { x }; f($c.v) = 9`) silently dropped the write.
//!
//! The question is a property of the *declaration*, exactly as
//! [`crate::runtime::Interpreter::routine_is_rw_capable`] is, so it is answered
//! from `ParamDef` rather than from anything about the argument value. It reads
//! the same `ParamDef::binds_caller_container` predicate the binder itself uses
//! to decide whether to install the shared cell, so the gate and the consumer
//! cannot disagree about what "binds a container" means.

use super::*;

impl Interpreter {
    /// Whether any registered candidate named `name` declares a parameter at
    /// positional index `positional` that binds the caller's container
    /// (`is rw`, `is raw`, or a sigil-less `\x`).
    ///
    /// Deliberately over-approximating across a `multi`'s candidates: which one
    /// a call lands on depends on argument *types*, and the arguments are still
    /// being evaluated when this runs. Over-approximating is the safe direction
    /// because the consumer is narrow — `try_fast_accessor_read`'s `want_ref`
    /// branch hands back a container only for a zero-argument read of a public
    /// `is rw` scalar attribute accessor, and every other dispatch ignores the
    /// flag — so a spurious `true` costs one attribute promotion, while a
    /// spurious `false` would silently switch the feature off.
    pub(crate) fn named_routine_binds_container_at(
        &mut self,
        name: &str,
        positional: usize,
    ) -> bool {
        let keys = self.fn_keys_for_base(name);
        self.keys_bind_container_at(&keys, positional)
    }

    /// [`Self::named_routine_binds_container_at`] for a caller that already
    /// holds the callee name's `Symbol`. `MarkRwArgRefContext` carries the name
    /// as a string constant, so `CompiledCode::const_sym` hands the symbol over
    /// and the `&str` form's `to_string()` + re-intern both disappear (#7766).
    pub(crate) fn named_routine_binds_container_at_sym(
        &mut self,
        name: &str,
        name_sym: crate::symbol::Symbol,
        positional: usize,
    ) -> bool {
        let keys = self.fn_keys_for_base_sym(name, name_sym);
        self.keys_bind_container_at(&keys, positional)
    }

    /// [`Self::named_routine_binds_container_at_sym`] that tells "no routine
    /// of this name is registered" (`None`) apart from "none binds a
    /// container" (`Some(false)`), for a caller with a fallback of its own —
    /// `RwArgCallee::Named`, which then asks the lexical `&name`.
    pub(crate) fn registered_routine_binds_container_at_sym(
        &mut self,
        name: &str,
        name_sym: crate::symbol::Symbol,
        positional: usize,
    ) -> Option<bool> {
        let keys = self.fn_keys_for_base_sym(name, name_sym);
        if keys.is_empty() {
            return None;
        }
        Some(self.keys_bind_container_at(&keys, positional))
    }

    /// Shared body of the entry points above.
    fn keys_bind_container_at(
        &mut self,
        keys: &[crate::symbol::Symbol],
        positional: usize,
    ) -> bool {
        if keys.is_empty() {
            return false;
        }
        let registry = self.registry();
        keys.iter().any(|k| {
            registry.functions.get(k).is_some_and(|def| {
                positional_binds_container_at(
                    def.param_defs.iter().filter(|p| !p.named),
                    positional,
                )
            })
        })
    }
}

/// Whether the `positional`-th of `params` (a signature's positional
/// parameters, in order) binds the caller's container. For
/// [`crate::opcode::RWARG_POSITIONAL_UNKNOWN`] — an argument after a `|slip`,
/// whose index is only known at run time — whether *any* of them does: the
/// over-approximating direction, as for a `multi`'s candidates.
pub(crate) fn positional_binds_container_at<'a>(
    mut params: impl Iterator<Item = &'a crate::ast::ParamDef>,
    positional: usize,
) -> bool {
    if positional == crate::opcode::RWARG_POSITIONAL_UNKNOWN as usize {
        return params.any(|p| p.binds_caller_container());
    }
    params
        .nth(positional)
        .is_some_and(|p| p.binds_caller_container())
}
