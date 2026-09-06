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
        if keys.is_empty() {
            return false;
        }
        let registry = self.registry();
        keys.iter().any(|k| {
            registry.functions.get(k).is_some_and(|def| {
                def.param_defs
                    .iter()
                    .filter(|p| !p.named)
                    .nth(positional)
                    .is_some_and(|p| p.binds_caller_container())
            })
        })
    }
}
