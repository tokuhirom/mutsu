use super::*;

impl Interpreter {
    pub(super) fn exec_post_increment_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        // Phase 3 Stage 2: scalar attribute increments read-modify-write the cell.
        let attr_name = Self::const_str(code, name_idx).to_string();
        self.sync_attr_local_from_cell_by_name(code, &attr_name);
        let r = self.exec_post_increment_op_inner(code, name_idx);
        if r.is_ok() {
            self.mirror_attr_local_to_cell_by_name(code, &attr_name);
        }
        r
    }

    /// Read-modify-write a scalar attribute that has NO local slot — the public
    /// accessor twigil form `$.count++` / `--$.count`. The private `$!count` form
    /// binds a local slot and is handled by the slot + cell-mirror path; the public
    /// form compiles to a raw name op (`.count`) with no slot, so without this the
    /// increment lands only in `env` and never reaches `self`'s shared attribute
    /// cell — losing the rw-attribute update under compiled dispatch (the hyper-rw
    /// `@objs».inc` of `method inc { $.count++ }`, #3658). Returns `Some(result)`
    /// when it handled a slotless scalar attribute, `None` otherwise (the caller
    /// continues its normal env path). `is_pre` selects whether the NEW (pre) or
    /// OLD (post) value is pushed onto the stack.
    pub(super) fn try_slotless_attr_incdec(
        &mut self,
        code: &CompiledCode,
        name: &str,
        is_increment: bool,
        is_pre: bool,
    ) -> Option<Result<(), RuntimeError>> {
        if Self::attr_twigil_base(name).is_none() || self.find_local_slot(code, name).is_some() {
            return None;
        }
        let cur = self.read_self_attr_cell(name)?;
        let result = (|| {
            self.check_readonly_for_increment(name)?;
            let val = self.normalize_incdec_source_with_type(name, cur);
            let new_val = if is_increment {
                self.increment_value_smart(&val)?
            } else {
                self.decrement_value_smart(&val)?
            };
            self.check_incdec_type_constraint(name, &new_val)?;
            self.write_self_attr_cell(name, new_val.clone());
            // Keep env coherent for a later same-frame read of the accessor name.
            self.set_env_with_main_alias(name, new_val.clone());
            self.stack.push(if is_pre { new_val } else { val });
            Ok(())
        })();
        Some(result)
    }

    fn exec_post_increment_op_inner(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        // Lazily convert pending alias bind names into local_bind_pairs.
        self.resolve_pending_alias_binds(code);
        let name = Self::const_str(code, name_idx);
        // Handle $CALLER::varname++ — increment through caller scope
        if let Some((bare_name, depth)) = crate::compiler::Compiler::parse_caller_prefix(name) {
            let raw_val = loan_env!(self, get_caller_var(&bare_name, depth))?;
            let val = Self::normalize_incdec_source(raw_val);
            let new_val = self.increment_value_smart(&val)?;
            loan_env!(self, set_caller_var(&bare_name, depth, new_val))?;
            self.stack.push(val);
            return Ok(());
        }
        if let Some(r) = self.try_slotless_attr_incdec(code, name, true, false) {
            return r;
        }
        self.check_readonly_for_increment(name)?;
        if name.starts_with('!')
            && let Some(slot) = self.find_local_slot(code, name)
            && !matches!(self.locals[slot], Value::Proxy { .. })
        {
            // ContainerRef: increment through the shared arc (e.g. `$!attr := outer_var`).
            if let Value::ContainerRef(ref arc) = self.locals[slot].clone() {
                if self.atomic_container_incdec(arc, name, true, true) {
                    return Ok(());
                }
                let inner = arc.lock().unwrap().clone();
                let val = self.normalize_incdec_source_with_type(name, inner);
                let new_val = self.increment_value_smart(&val)?;
                arc.lock().unwrap().clone_from(&new_val);
                self.stack.push(val);
                return Ok(());
            }
            let raw_val = self.locals[slot].clone();
            let val = self.normalize_incdec_source_with_type(name, raw_val);
            let new_val = self.increment_value_smart(&val)?;
            self.locals[slot] = new_val.clone();
            self.flush_local_to_env(code, slot);
            // Propagate the new value along the sigilless alias chain and into
            // self's shared cell for an attribute-twigil alias.
            self.propagate_sigilless_alias_chain(code, name, &new_val);
            self.stack.push(val);
            return Ok(());
        }
        let raw_val = self
            .get_env_with_main_alias(name)
            .or_else(|| self.anon_state_value(name))
            .unwrap_or(Value::Int(0));
        // ContainerRef: deref for increment, write back through the shared container.
        // Use an atomic read-modify-write under the cell lock so concurrent
        // `start { $shared++ }` blocks don't lose updates (Track C).
        if let Value::ContainerRef(ref arc) = raw_val {
            if self.atomic_container_incdec(arc, name, true, true) {
                return Ok(());
            }
            let inner = arc.lock().unwrap().clone();
            let val = self.normalize_incdec_source_with_type(name, inner);
            let new_val = self.increment_value_smart(&val)?;
            arc.lock().unwrap().clone_from(&new_val);
            self.stack.push(val);
            return Ok(());
        }
        // If the value is a Proxy, FETCH → increment → STORE
        if let Value::Proxy { storer, .. } = &raw_val
            && !matches!(storer.as_ref(), Value::Nil)
        {
            let fetched = loan_env!(self, auto_fetch_proxy(&raw_val))?;
            let val = Self::normalize_incdec_source(fetched);
            let new_val = self.increment_value_smart(&val)?;
            loan_env!(self, assign_proxy_lvalue(raw_val, new_val))?;
            self.stack.push(val);
            return Ok(());
        }
        let val = self.normalize_incdec_source_with_type(name, raw_val);
        let new_val = self.increment_value_smart(&val)?;
        self.store_named_scalar_rmw_result(code, name, new_val)?;
        self.stack.push(val);
        Ok(())
    }

    pub(super) fn exec_post_decrement_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        // Phase 3 Stage 2: scalar attribute decrements read-modify-write the cell.
        let attr_name = Self::const_str(code, name_idx).to_string();
        self.sync_attr_local_from_cell_by_name(code, &attr_name);
        let r = self.exec_post_decrement_op_inner(code, name_idx);
        if r.is_ok() {
            self.mirror_attr_local_to_cell_by_name(code, &attr_name);
        }
        r
    }

    fn exec_post_decrement_op_inner(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        let name = Self::const_str(code, name_idx);
        if let Some(r) = self.try_slotless_attr_incdec(code, name, false, false) {
            return r;
        }
        self.check_readonly_for_increment(name)?;
        if name.starts_with('!')
            && let Some(slot) = self.find_local_slot(code, name)
            && !matches!(self.locals[slot], Value::Proxy { .. })
        {
            // ContainerRef: decrement through the shared arc (e.g. `$!attr := outer_var`).
            if let Value::ContainerRef(ref arc) = self.locals[slot].clone() {
                if self.atomic_container_incdec(arc, name, false, true) {
                    return Ok(());
                }
                let inner = arc.lock().unwrap().clone();
                let val = self.normalize_incdec_source_with_type(name, inner);
                let new_val = self.decrement_value_smart(&val)?;
                arc.lock().unwrap().clone_from(&new_val);
                self.stack.push(val);
                return Ok(());
            }
            let raw_val = self.locals[slot].clone();
            let val = self.normalize_incdec_source_with_type(name, raw_val);
            let new_val = self.decrement_value_smart(&val)?;
            self.locals[slot] = new_val.clone();
            self.flush_local_to_env(code, slot);
            // Propagate the new value along the sigilless alias chain and into
            // self's shared cell for an attribute-twigil alias.
            self.propagate_sigilless_alias_chain(code, name, &new_val);
            self.stack.push(val);
            return Ok(());
        }
        let raw_val = self
            .get_env_with_main_alias(name)
            .or_else(|| self.anon_state_value(name))
            .unwrap_or(Value::Int(0));
        // ContainerRef: deref for decrement, write back through the shared container.
        // Atomic RMW under the cell lock for concurrent `start` blocks (Track C).
        if let Value::ContainerRef(ref arc) = raw_val {
            if self.atomic_container_incdec(arc, name, false, true) {
                return Ok(());
            }
            let inner = arc.lock().unwrap().clone();
            let val = self.normalize_incdec_source_with_type(name, inner);
            let new_val = self.decrement_value_smart(&val)?;
            arc.lock().unwrap().clone_from(&new_val);
            self.stack.push(val);
            return Ok(());
        }
        let val = self.normalize_incdec_source_with_type(name, raw_val);
        let new_val = self.decrement_value_smart(&val)?;
        self.store_named_scalar_rmw_result(code, name, new_val)?;
        self.stack.push(val);
        Ok(())
    }

    pub(super) fn exec_post_increment_index_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        self.exec_inc_dec_index_op(code, name_idx, true, false)
    }

    pub(super) fn exec_post_decrement_index_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        self.exec_inc_dec_index_op(code, name_idx, false, false)
    }

    pub(super) fn exec_pre_increment_index_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        self.exec_inc_dec_index_op(code, name_idx, true, true)
    }

    pub(super) fn exec_pre_decrement_index_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        self.exec_inc_dec_index_op(code, name_idx, false, true)
    }
}
