//! ADR-0067's argument producer for a callee with **no compile-time name**.
//!
//! `sub g($y is rw) { $y = 9 }; g($c.v)` was closed by the
//! returned-container-consumers slice, whose gate
//! (`OpCode::MarkRwArgRefContext`) keys on the callee's *name*. Two spellings
//! have no usable name, and both died with "expects a writable container"
//! where raku writes `9`:
//!
//! ```raku
//! class Sink { method take($y is rw) { $y = 9 } }
//! Sink.new.take($c.v);            # the invocant's class is not known at compile time
//! my $r = &g; $r($c.v);           # the callee is a runtime value
//! ```
//!
//! The ticket that recorded them proposed either a program-wide "any user
//! method declares a container-binding parameter" flag or a name-keyed method
//! index. Measured with `--dump-bytecode`, **neither is necessary**: every one
//! of these spellings pushes its callee *before* its arguments —
//!
//! ```text
//! $s.take($c.v)      GetBareWord/GetLocal(<$s>); CallMethodMut{"new"};   <- the invocant
//!                    GetLocal(<$c>); <marker>; CallMethodMut{"v"}; CallMethod{"take"}
//! $r($c.v)           GetLocal(<$r>);                                     <- the code object
//!                    GetLocal(<$c>); <marker>; CallMethodMut{"v"}; CallOnValue
//! ```
//!
//! — so at the instant the marker runs the real callee is already on the stack,
//! one slot below the argument being compiled plus one for each earlier
//! argument (each leaves exactly one value, named ones included; that count is
//! `RwArgCalleeMark::stack_offset`, which is why it is tracked separately from
//! the *signature* index `positional`, the one a named argument does not
//! consume). The gate therefore asks the **actual** callee's signature instead
//! of over-approximating over every routine that shares a name, which is both
//! exact and, for the code-value case, free: a `SubData` carries its own
//! `param_defs`.
//!
//! One pre-filter is still needed, and only for the method half: resolving a
//! method costs an MRO walk, so `Registry::any_container_binding_method_param`
//! (set-only, raised by the same writer that raises slice 3a's raw-invocant
//! flag) short-circuits it in the overwhelming majority of programs. That flag
//! is used as a *filter*, never as the answer.

use super::*;
use crate::opcode::{RwArgCallee, RwArgCalleeMark};

impl Interpreter {
    /// The gate behind [`OpCode::MarkRwArgRefContextCallee`]: does the callee
    /// this marker points at bind its `positional`-th positional argument to
    /// the caller's container?
    pub(super) fn rw_arg_callee_binds_container(
        &mut self,
        code: &CompiledCode,
        mark: &RwArgCalleeMark,
    ) -> bool {
        let positional = mark.positional as usize;
        let stack_offset = mark.stack_offset as usize;
        match &mark.callee {
            RwArgCallee::Method { name_idx } => {
                // The MRO walk below is the only expensive branch, and a
                // container-binding method parameter is rare; skip it whole
                // when no program-wide declaration could satisfy it. The filter
                // is asked BEFORE the stack read and the method-name allocation
                // for the reason slice 3a measured: most of that gate's cost was
                // the argument extraction, not the resolve.
                if !self.registry().any_container_binding_method_param {
                    // ...but a spurious `false` would silently switch the
                    // feature off, so a debug build re-derives the slow answer
                    // and fails the `t/` suite if a future registration path
                    // ever bypasses `Registry::note_container_binding_methods`.
                    #[cfg(debug_assertions)]
                    if let Some(invocant) = self.rw_arg_callee_stack_value(stack_offset) {
                        let method = Self::const_str(code, *name_idx).to_string();
                        debug_assert!(
                            !self.method_binds_container_at(&invocant, &method, positional),
                            "any_container_binding_method_param is false but {method:?} \
                             binds positional {positional} to a container -- \
                             a writer bypassed note_container_binding_methods"
                        );
                    }
                    return false;
                }
                let Some(invocant) = self.rw_arg_callee_stack_value(stack_offset) else {
                    return false;
                };
                let method = Self::const_str(code, *name_idx).to_string();
                self.method_binds_container_at(&invocant, &method, positional)
            }
            RwArgCallee::Code => {
                let Some(callee) = self.rw_arg_callee_stack_value(stack_offset) else {
                    return false;
                };
                Self::code_value_binds_container_at(&callee, positional)
            }
            RwArgCallee::CodeVar { name_idx } => {
                let name = Self::const_str(code, *name_idx).to_string();
                let callee = self.resolve_rw_arg_code_var(code, &name);
                // A `&g(...)` whose code variable resolves to a real code
                // object is answered exactly, like the `Code` arm. When it does
                // not (a routine declared after its use site, which is why the
                // named gate resolves at run time too), fall back to the
                // by-name registry question `MarkRwArgRefContext` asks.
                match callee {
                    Some(callee) if matches!(callee.view(), ValueView::Sub(_)) => {
                        Self::code_value_binds_container_at(&callee, positional)
                    }
                    _ => self.named_routine_binds_container_at(&name, positional),
                }
            }
        }
    }

    /// [`OpCode::IndexArgRef`]'s gate: the same question one stack slot deeper,
    /// plus the topic rule an explicit signature cannot express.
    ///
    /// The subscript's own target and index are still on the stack when this
    /// runs, so the callee sits one slot below where the accessor marker finds
    /// it. And a bare block declares no parameter at all yet still binds its
    /// implicit `$_` RAW to its argument (`my $b = { $_ = 9 }; $b(@a[0])` writes
    /// `@a`), which is why the signature question alone is not enough.
    pub(super) fn index_arg_callee_binds_container(
        &mut self,
        code: &CompiledCode,
        mark: &crate::opcode::IndexArgRefMark,
    ) -> bool {
        let deeper = crate::opcode::RwArgCalleeMark {
            positional: mark.mark.positional,
            stack_offset: mark.mark.stack_offset + 1,
            callee: mark.mark.callee.clone(),
        };
        if self.rw_arg_callee_binds_container(code, &deeper) {
            return true;
        }
        if mark.mark.positional != 0 {
            return false;
        }
        let callee = match &deeper.callee {
            crate::opcode::RwArgCallee::Code => {
                self.rw_arg_callee_stack_value(deeper.stack_offset as usize)
            }
            crate::opcode::RwArgCallee::CodeVar { name_idx } => {
                let name = Self::const_str(code, *name_idx).to_string();
                self.resolve_rw_arg_code_var(code, &name)
            }
            // A method never binds an argument to the topic.
            crate::opcode::RwArgCallee::Method { .. } => None,
        };
        callee.is_some_and(|c| Self::code_value_binds_topic_raw(&c))
    }

    /// Whether a code value is a bare block whose implicit `$_` binds its sole
    /// argument RAW — the topic half of the container question.
    ///
    /// Mirrors the branch that actually performs that binding in
    /// `call_compiled_closure_in_unit`: a bare block with no positional
    /// parameter of its own, whose body does not read `@_` instead of `$_`.
    fn code_value_binds_topic_raw(callee: &Value) -> bool {
        let callee = callee.deref_container();
        let ValueView::Sub(data) = callee.view() else {
            return false;
        };
        data.is_bare_block
            && data.param_defs.is_empty()
            && !data.params.iter().any(|p| p != "_" && !p.starts_with(':'))
            && !crate::method_signature_shared::auto_signature_uses(&data.body).0
    }

    /// The callee's stack slot for a marker whose argument sits `stack_offset`
    /// values above it. `None` rather than a panic when the stack is shorter
    /// than the layout implies — a producer that cannot find its callee must
    /// decline, never abort a program that is otherwise correct.
    pub(super) fn rw_arg_callee_stack_value(&self, stack_offset: usize) -> Option<Value> {
        let depth = stack_offset + 2;
        self.stack
            .len()
            .checked_sub(depth)
            .and_then(|idx| self.stack.get(idx))
            .cloned()
    }

    /// Whether a code *value*'s own signature binds its `positional`-th
    /// positional parameter to the caller's container. Reads `SubData`'s
    /// `param_defs` directly, so this is exact and needs no registry lookup.
    fn code_value_binds_container_at(callee: &Value, positional: usize) -> bool {
        let callee = callee.deref_container();
        let ValueView::Sub(data) = callee.view() else {
            return false;
        };
        data.param_defs
            .iter()
            .filter(|p| !p.named && !p.is_invocant)
            .nth(positional)
            .is_some_and(|p| p.binds_caller_container())
    }

    /// Whether any candidate reachable from `invocant`'s MRO under `method`
    /// binds its `positional`-th positional parameter to the caller's
    /// container.
    ///
    /// Deliberately a *candidate-set* question rather than `resolve_method`:
    /// the call's arguments are still being evaluated when this runs, so a
    /// typed `multi` candidate could not be selected yet. Over-approximating
    /// within the receiver's own MRO is the safe direction (the consumer is
    /// narrow — see this module's header).
    fn method_binds_container_at(
        &mut self,
        invocant: &Value,
        method: &str,
        positional: usize,
    ) -> bool {
        let invocant = invocant.deref_container();
        let class_name = Self::raw_invocant_class_name(&invocant);
        let name = Symbol::intern(method);
        let mro = self.class_mro(&class_name);
        let registry = self.registry();
        mro.iter()
            .any(|owner| registry.any_method_binds_container_at(*owner, name, positional))
    }

    /// Resolve `&name` the way `exec_call_on_code_var_op` does: the env first,
    /// then this frame's own `&`-sigil local slot (which is how a `&`-sigil
    /// named parameter binds). Deliberately without that function's `&!attr`
    /// instance fallback — an attribute-held callable is not a shape this
    /// producer can reach anyway, and re-deriving it here would be a second
    /// copy of a rule with one owner.
    fn resolve_rw_arg_code_var(&mut self, code: &CompiledCode, name: &str) -> Option<Value> {
        let target = loan_env!(self, resolve_code_var(name));
        if !target.is_nil() {
            return Some(target);
        }
        let slot = self.find_local_slot(code, &format!("&{name}"))?;
        self.locals.get(slot).filter(|v| !v.is_nil()).cloned()
    }
}
