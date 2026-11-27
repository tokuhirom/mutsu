//! Per-site bodies of the control-flow opcodes that both `exec_one_dispatch`
//! and a Tier A JIT shim (`vm_jit_helpers.rs`) execute: `Return` and the
//! conditional jumps. Split out of `vm_call_site_ops` (which keeps the call
//! opcodes) under the same contract: each function is the whole behavior of
//! its opcode at one site and never advances `ip`.

use super::*;

impl Interpreter {
    /// `OpCode::Return`. `Ok(())` means a lexically rebound `&return` ran and
    /// pushed its result, so execution continues at the next opcode; otherwise
    /// the return signal (or the rebound call's error) is returned.
    pub(super) fn exec_return_site(&mut self, code: &CompiledCode) -> Result<(), RuntimeError> {
        let val = self.stack.pop().unwrap_or(Value::NIL);
        // Check if &return has been lexically rebound; if so, call the rebound
        // function instead of performing a built-in return. Pre-interned
        // (`wk::rebound_return`) and gated on the process-global latch
        // (`env::return_rebound_possible`): this runs on every return, and with
        // no rebinding anywhere the lookup is a miss that walks every overlay
        // tier plus the global base (~2% of `bench-fib`).
        if crate::env::return_rebound_possible()
            && let Some(rebound) = self
                .env()
                .get_sym(crate::symbol::wk::rebound_return())
                .cloned()
            && matches!(
                rebound.view(),
                ValueView::Sub(_) | ValueView::WeakSub(_) | ValueView::Routine { .. }
            )
        {
            let result = self.vm_call_on_value(rebound, vec![val], None)?;
            self.stack.push(result);
            return Ok(());
        }
        // Under `use fatal` -- which a `try` block turns on for its body -- a
        // Failure is thrown where it is produced into the fatal scope, so
        // `try { return $s.Num }` with a non-numeric `$s` lands in the try
        // (Rakudo: the try yields Nil and the routine carries on) instead of
        // returning the Failure (Template::Jinja2's `float` filter).
        if self.module.fatal_mode
            && let Some(failure) = self.failure_to_runtime_error_if_unhandled(&val)
        {
            return Err(failure);
        }
        let mut err = RuntimeError::return_signal(val);
        // ADR-0037 Slice 4: an EVAL unit whose `context => $ctx` named a live
        // routine bakes that routine's id onto its own `CompiledCode`
        // (`compile_block_value_opts`); stamp it here so the signal unwinds
        // past any intervening routine boundary to the frame this id names,
        // instead of being caught by the first one it reaches.
        if let Some(target_id) = code.eval_context_target_callable_id {
            err.set_return_target_callable_id(Some(target_id));
        }
        Err(err)
    }

    /// `OpCode::JumpIfFalse` condition: pops the tested value and reports
    /// whether the jump is taken (falsy). Failures tested for truthiness (the
    /// `&&` operator) are marked handled -- the original below the dup too,
    /// on the taken path.
    #[inline]
    #[cfg(feature = "jit")]
    pub(super) fn jump_if_false_taken(&mut self) -> bool {
        Self::mark_failure_handled_on_stack(&mut self.stack);
        let val = self.stack.pop().unwrap();
        if !self.eval_truthy(&val) {
            Self::mark_failure_handled_on_stack(&mut self.stack);
            true
        } else {
            false
        }
    }

    /// [`Self::jump_if_false_taken`] for the interpreter loop: an error the
    /// tested value's own `Bool` raised unwinds the statement instead of
    /// being swallowed (the JIT helper keeps the infallible form).
    #[inline]
    pub(super) fn try_jump_if_false_taken(&mut self) -> Result<bool, RuntimeError> {
        Self::mark_failure_handled_on_stack(&mut self.stack);
        let val = self.stack.pop().unwrap();
        if !self.eval_truthy_result(&val)? {
            Self::mark_failure_handled_on_stack(&mut self.stack);
            Ok(true)
        } else {
            Ok(false)
        }
    }

    /// [`Self::jump_if_true_taken`], erroring like
    /// [`Self::try_jump_if_false_taken`].
    #[inline]
    pub(super) fn try_jump_if_true_taken(&mut self) -> Result<bool, RuntimeError> {
        Self::mark_failure_handled_on_stack(&mut self.stack);
        let val = self.stack.last().unwrap().clone();
        self.eval_truthy_result(&val)
    }

    /// `OpCode::JumpIfTrue` condition: PEEKS the tested value (it stays on the
    /// stack on both paths -- `||`-style short-circuit); true when truthy.
    #[inline]
    #[cfg(feature = "jit")]
    pub(super) fn jump_if_true_taken(&mut self) -> bool {
        Self::mark_failure_handled_on_stack(&mut self.stack);
        let val = self.stack.last().unwrap().clone();
        self.eval_truthy(&val)
    }

    /// `OpCode::JumpIfNotNil` condition: PEEKS the tested value (kept on the
    /// stack on both paths -- `//`-style short-circuit); true when defined.
    #[inline]
    pub(super) fn jump_if_not_nil_taken(&mut self) -> bool {
        Self::mark_failure_handled_on_stack(&mut self.stack);
        let val = self.stack.last().unwrap().clone();
        self.value_is_defined_dispatch(&val)
    }

    /// `OpCode::StateVarInitGuard` condition, keyed on the opcode's state key:
    /// when the state var is already initialized, pushes the `NIL` placeholder
    /// `StateVarInit` discards and returns true (skip the RHS initializer);
    /// otherwise returns false (fall through and run it).
    #[inline]
    pub(super) fn state_var_init_guard_taken(&mut self, base_key: crate::symbol::Symbol) -> bool {
        let scoped_key = self.scoped_state_key(base_key);
        if self.get_state_var(scoped_key).is_some() {
            self.stack.push(Value::NIL);
            true
        } else {
            false
        }
    }
}
