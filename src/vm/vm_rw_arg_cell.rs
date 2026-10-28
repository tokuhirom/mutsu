//! `OpCode::CaptureRwArgCell`: the container an lvalue call relays to an
//! `is rw` parameter (#11077).
//!
//! `f($h) = 1` and `++f($h)` are rewritten by the parser into
//! `__mutsu_assign_named_sub_lvalue("f", [$h], 1)` /
//! `__mutsu_incdec_named_sub_lvalue(...)`, so `f`'s arguments travel inside a
//! list literal. `MakeArray` boxes a `$`-scalar element into its shared cell
//! only when the variable holds a plain value; a Hash/Array/object reaches the
//! routine bare, and an `is rw` parameter rejects it as "a value without a
//! container". This op boxes the variable the way the direct call's binder
//! does, so both spellings bind the same Scalar.

use super::*;

impl Interpreter {
    // Cost: O(1) with the compiler's slot hint; O(l) by-name locals fallback, l = frame locals.
    pub(super) fn exec_capture_rw_arg_cell_op(&mut self, code: &CompiledCode) {
        let val = self.stack.pop().unwrap_or(Value::NIL);
        let ValueView::VarRef {
            name, value: inner, ..
        } = val.view()
        else {
            self.stack.push(val);
            return;
        };
        let source_name = name.resolve();
        // A readonly binding (`my $b := 42`, a non-rw parameter) has no
        // container to alias: hand the binder the VarRef unchanged so it
        // reports the binding the way a direct call does.
        if self.name_is_readonly_binding(&source_name) {
            self.stack.push(val);
            return;
        }
        let slot_hint = val.varref_slot();
        let cell = self.capture_rw_arg_cell(code, &source_name, inner.clone(), slot_hint);
        self.register_container_cell_constraint_for_name(&cell, &source_name);
        self.stack.push(cell);
    }

    /// See [`OpCode::MarkReadonlyRwTail`].
    // Cost: O(1).
    pub(super) fn exec_mark_readonly_rw_tail_op(&mut self) {
        if let Some(top) = self.stack.last()
            && matches!(top.view(), ValueView::Hash(..) | ValueView::Array(..))
        {
            self.lexicals.readonly_rw_tail = Some(top.clone());
        }
    }

    /// Box each `is rw` positional `VarRef` argument of `callable` into the
    /// caller's shared cell, the way the direct-call binder does. A wrapper's
    /// `|c` capture, or a native body's out-parameter write-back, can only
    /// reach the caller's variable through its container (#12521). The
    /// signature is the routine's own, not a registry lookup by call name: an
    /// imported routine (`EXPORT` returning `&sub`) is not registered under
    /// the name it is called by.
    // Cost: O(p + l), p = parameters of the callable, l = frame locals (the by-name slot fallback).
    pub(super) fn box_rw_args_for_callable(
        &mut self,
        code: &CompiledCode,
        callable: &Value,
        args: &mut [Value],
        arg_sources: &Option<Vec<Option<String>>>,
    ) {
        let callable = Self::unwrap_callable_mixin(callable.clone());
        let ValueView::Sub(sub) = callable.view() else {
            return;
        };
        for (idx, pd) in sub.param_defs.iter().enumerate() {
            if pd.named || !pd.traits.iter().any(|t| t == "rw") {
                continue;
            }
            let Some(arg) = args.get(idx) else { break };
            let slot_hint = arg.varref_slot();
            let (var, inner) = if let ValueView::VarRef { name, value, .. } = arg.view() {
                (name.resolve().to_string(), value.clone())
            } else if !arg.is_container_ref()
                && let Some(Some(source)) = arg_sources
                    .as_ref()
                    .filter(|s| s.len() == args.len())
                    .and_then(|s| s.get(idx))
                && source
                    .as_bytes()
                    .first()
                    .is_some_and(|b| b.is_ascii_alphabetic() || *b == b'_')
            {
                // A call site that hands over plain values and names the
                // caller's variable separately (an `EXPORT`-installed `&sub`).
                (source.clone(), arg.clone())
            } else {
                continue;
            };
            if self.name_is_readonly_binding(&var) {
                continue;
            }
            let cell = self.capture_rw_arg_cell(code, &var, inner, slot_hint);
            self.register_container_cell_constraint_for_name(&cell, &var);
            args[idx] = cell;
        }
    }

    /// Publish the caller's shared cell for a scalar `is rw`/`is raw` argument
    /// into env before the general binder runs (#12907).
    ///
    /// A frame entered through the light path keeps an `is rw` parameter only in
    /// its local slot, as a shared `ContainerRef` cell (ADR-0109); env has no
    /// entry for it. The binder looks the caller's cell up by name in env, found
    /// none and boxed a fresh cell, so a write by the callee never reached the
    /// caller's variable (`sub top($l is rw) { typed($l) }`).
    // Cost: O(p), p = parameters of the callee.
    pub(super) fn publish_rw_arg_cells(
        &mut self,
        param_defs: &[crate::ast::ParamDef],
        args: &[Value],
    ) {
        for (idx, pd) in param_defs
            .iter()
            .filter(|pd| !pd.is_invocant && !pd.named)
            .enumerate()
        {
            if !pd.traits.iter().any(|t| t == "rw" || t == "raw") {
                continue;
            }
            let Some(arg) = args.get(idx) else { break };
            let Some(slot) = arg.varref_slot().filter(|slot| *slot != u32::MAX) else {
                continue;
            };
            let ValueView::VarRef { name, .. } = arg.view() else {
                continue;
            };
            let Some(cell) = self
                .locals
                .get(slot as usize)
                .filter(|v| v.is_container_ref())
            else {
                continue;
            };
            let cell = cell.clone();
            self.env_mut().insert(name.resolve().to_string(), cell);
        }
    }

    /// [`Self::publish_rw_arg_cells`] for a call whose arguments carry no
    /// `VarRef` (a method call): the caller's argument variables are named by
    /// the call site's slot map instead.
    // Cost: O(s), s = slotted argument sources of the call.
    pub(crate) fn publish_rw_arg_cells_by_source(&mut self) {
        if self.pending_call_arg_source_slots.is_empty() {
            return;
        }
        let cells: Vec<(crate::symbol::Symbol, Value)> = self
            .pending_call_arg_source_slots
            .iter()
            .filter(|(_, slot)| **slot != u32::MAX)
            .filter_map(|(name, slot)| {
                let cell = self.locals.get(*slot as usize)?;
                cell.is_container_ref().then(|| (*name, cell.clone()))
            })
            .collect();
        for (name, cell) in cells {
            self.env_mut().insert(name.resolve().to_string(), cell);
        }
    }
}
