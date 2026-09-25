//! `++$x`, `$x++`, `--$x`, `$x--` on a named scalar: one body for all four.
//!
//! The four `PreIncrement`/`PostIncrement`/`PreDecrement`/`PostDecrement`
//! opcodes used to carry four separate bodies in two files, and they drifted:
//! only `$x++` handled `$CALLER::x` and pending alias binds, only `$x++`
//! FETCHed and STOREd through a `Proxy`, `$x--` reported a readonly operand as
//! `postfix:<++>`, and the pre/post pair walked two different sigilless-alias
//! helpers (#9450). They differ in exactly two things -- which way the value
//! steps, and whether the old or the new value is the expression's result --
//! so that is all [`IncDec`] carries.
//!
//! The indexed forms (`@a[$i]++` and friends) already share one body,
//! `exec_inc_dec_index_op`.

use super::*;

/// Which of the four operators: the step direction and the result.
#[derive(Clone, Copy, Debug)]
pub(crate) struct IncDec {
    /// `++` (true) or `--` (false).
    pub(crate) increment: bool,
    /// Prefix (the result is the NEW value) or postfix (the OLD value).
    pub(crate) prefix: bool,
}

impl IncDec {
    /// The operator's routine name, as a readonly-operand error reports it.
    fn routine_name(self) -> &'static str {
        match (self.prefix, self.increment) {
            (true, true) => "prefix:<++>",
            (true, false) => "prefix:<-->",
            (false, true) => "postfix:<++>",
            (false, false) => "postfix:<-->",
        }
    }

    /// The expression's value: the new value for prefix, the old for postfix.
    fn result(self, old: Value, new: Value) -> Value {
        if self.prefix { new } else { old }
    }
}

impl Interpreter {
    /// One step of `++`/`--` on a value, with the smart string/Int rules.
    fn incdec_step(&mut self, k: IncDec, val: &Value) -> Result<Value, RuntimeError> {
        if k.increment {
            self.increment_value_smart(val)
        } else {
            self.decrement_value_smart(val)
        }
    }

    /// The four scalar `++`/`--` opcodes. A scalar attribute is read-modify-
    /// written through its cell (Phase 3 Stage 2).
    // Cost: O(1) for a local or an env scalar; O(d) for `$CALLER::x`, d = frames
    // walked; plus the sigilless alias chain when one was ever registered.
    pub(super) fn exec_scalar_incdec_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        slot: Option<u32>,
        k: IncDec,
    ) -> Result<(), RuntimeError> {
        let attr_name = Self::const_str(code, name_idx).to_string();
        self.sync_attr_local_from_cell_by_name(code, &attr_name);
        let r = self.exec_scalar_incdec_inner(code, name_idx, slot, k);
        if r.is_ok() {
            self.mirror_attr_local_to_cell_by_name(code, &attr_name);
        }
        r
    }

    fn exec_scalar_incdec_inner(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        slot: Option<u32>,
        k: IncDec,
    ) -> Result<(), RuntimeError> {
        // Lazily convert pending alias bind names into local_bind_pairs.
        self.resolve_pending_alias_binds(code);
        let name = Self::const_str(code, name_idx);
        // The name's interned form, memoized per chunk: the readonly registry,
        // the env, and the type-constraint lane are all Symbol-keyed, and this
        // op probes each of them once per step.
        let name_sym = code.const_sym(name_idx);
        // `$CALLER::x++` and friends step the caller's variable.
        if let Some((bare_name, depth)) = crate::compiler::Compiler::parse_caller_prefix(name) {
            let raw_val = loan_env!(self, get_caller_var(&bare_name, depth))?;
            let old = Self::normalize_incdec_source(raw_val);
            let new = self.incdec_step(k, &old)?;
            loan_env!(self, set_caller_var(&bare_name, depth, new.clone()))?;
            self.stack.push(k.result(old, new));
            return Ok(());
        }
        // `++$n` on a read-only parameter (non-`is rw`/`is copy`) is an
        // X::Multi::NoMatch in Raku: the operator requires a mutable argument.
        self.check_readonly_for_incdec_for(name, Some(name_sym), k.routine_name())?;
        if let Some(r) = self.try_slotless_attr_incdec(code, name, k.increment, k.prefix) {
            return r;
        }
        if name.starts_with('!')
            && let Some(slot) = self.find_local_slot(code, name)
            && !matches!(self.locals[slot].view(), ValueView::Proxy { .. })
        {
            // ContainerRef: step through the shared cell (e.g. `$!attr := outer_var`).
            let local_val = self.locals[slot].clone();
            if let ValueView::ContainerRef(arc) = local_val.view() {
                return self.incdec_through_cell(&arc, name, name_sym, k);
            }
            let raw_val = self.locals[slot].clone();
            let old = self.normalize_incdec_source_with_type_for(name, Some(name_sym), raw_val);
            let new = self.incdec_step(k, &old)?;
            let new = self.wrap_native_int_arithmetic_result_for(name, Some(name_sym), new);
            self.locals[slot] = new.clone();
            self.flush_local_to_env(code, slot);
            // Propagate the new value along the sigilless alias chain and into
            // self's shared cell for an attribute-twigil alias.
            self.propagate_sigilless_alias_chain(code, name, Some(slot), &new);
            self.stack.push(k.result(old, new));
            return Ok(());
        }
        let raw_val = self
            // A per-call anonymous state reads from the state store, never from
            // the stale env copy -- see `anon_state_key`.
            .per_call_anon_state_read(name, Value::int(0))
            .or_else(|| self.escaping_our_write_cell(code, name))
            .or_else(|| self.package_scope_lexical(name))
            .or_else(|| self.get_env_with_main_alias_sym(name, name_sym))
            .or_else(|| self.read_package_scope_var(name))
            .or_else(|| self.anon_state_value(name))
            .unwrap_or(Value::int(0));
        // ContainerRef (box-on-capture / `:=`): mutate the shared cell in place,
        // atomically under its lock so concurrent `start { $shared++ }` blocks
        // don't lose updates (Track C), and so closures over this lexical
        // observe the change.
        if let ValueView::ContainerRef(arc) = raw_val.view() {
            return self.incdec_through_cell(&arc, name, name_sym, k);
        }
        // A Proxy: FETCH, step, STORE.
        if let ValueView::Proxy { storer, .. } = raw_val.view()
            && !storer.is_nil()
        {
            let fetched = loan_env!(self, auto_fetch_proxy(&raw_val))?;
            let old = Self::normalize_incdec_source(fetched);
            let new = self.incdec_step(k, &old)?;
            let new = self.wrap_native_int_arithmetic_result_for(name, Some(name_sym), new);
            loan_env!(self, assign_proxy_lvalue(raw_val, new.clone()))?;
            self.stack.push(k.result(old, new));
            return Ok(());
        }
        let old = self.normalize_incdec_source_with_type_for(name, Some(name_sym), raw_val);
        let new = self.incdec_step(k, &old)?;
        let new =
            self.wrap_native_int_arithmetic_result_for_slot(code, slot, name, Some(name_sym), new);
        let stored = self.store_named_scalar_rmw_result(code, name, Some(name_sym), slot, new)?;
        self.stack.push(k.result(old, stored));
        Ok(())
    }

    /// Step a value held in a shared `ContainerCell`: atomically under the
    /// cell lock when the cell allows it, otherwise read, step and write back.
    fn incdec_through_cell(
        &mut self,
        arc: &crate::gc::Gc<crate::value::ContainerCell>,
        name: &str,
        name_sym: Symbol,
        k: IncDec,
    ) -> Result<(), RuntimeError> {
        if self.atomic_container_incdec(arc, name, k.increment, !k.prefix)? {
            return Ok(());
        }
        let inner = arc.lock().unwrap().clone();
        let old = self.normalize_incdec_source_with_type_for(name, Some(name_sym), inner);
        let new = self.incdec_step(k, &old)?;
        let new = self.wrap_native_int_arithmetic_result_for(name, Some(name_sym), new);
        arc.lock().unwrap().clone_from(&new);
        self.stack.push(k.result(old, new));
        Ok(())
    }
}
