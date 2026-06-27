use super::*;

impl Interpreter {
    pub(super) fn exec_set_var_dynamic_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        dynamic: bool,
    ) {
        let name = Self::const_str(code, name_idx);
        loan_env!(self, set_var_dynamic(name, dynamic));
        // A fresh declaration without an explicit type must not inherit stale
        // constraints from an earlier lexical with the same name.
        self.vm_set_var_type_constraint(name, None);
        if !name.starts_with('@') && !name.starts_with('%') && !name.starts_with('&') {
            loan_env!(self, reset_atomic_var_key_decl(name));
        }
        // A fresh @-variable declaration must clear any CAS atomic array
        // state left over from a previous lexical with the same name
        // (e.g. when `my @arr[N]` is declared inside a loop body).
        if name.starts_with('@') {
            self.clear_atomic_array_state(name);
        } else if name.starts_with('%') {
            self.clear_atomic_hash_state(name);
        }
        // Before this loop-body-local declaration overwrites the env entry for
        // `name`, record the outer value it shadows so the enclosing same-named
        // binding can be restored when the loop exits. Conditions are excluded
        // (`loop_cond_active`): a `my` in a `while`/`until`/`loop` condition is
        // enclosing-scoped and often read after the loop. Only record names that
        // already had a binding (a genuine shadow — there is nothing to clobber
        // otherwise), and only the first time in this loop scope so the saved
        // value is the pre-loop one. See Interpreter::loop_local_saved_env.
        //
        // Crucially, only record names the compiler scoped to the loop body — i.e.
        // names with a local slot in `code.locals`. A `my` in a *statement
        // modifier* loop (`(my @a).push: $_ for ^3`) introduces no block, so the
        // compiler scopes it to the *enclosing* unit as an env-only var with no
        // local slot; it is enclosing-scoped (read after the loop) and must NOT be
        // restored. Restoring it would wipe an accumulated `0,1,2` back to its
        // first-iteration value. Genuine body-local shadows (`for {...{ my @a }}`)
        // always get a slot, so this distinguishes the two reliably.
        if !self.loop_cond_active
            && let Some(prev) = self.env().get(name).cloned()
        {
            // Only record a *genuine, live* enclosing binding for restoration.
            // The restore writes back both env and the local slot, so the value
            // being shadowed must be a real outer binding that is backed by a
            // local slot whose value is coherent with env. A leaked or freshly
            // hoisted env entry is NOT: e.g. a statement-modifier `(my @a)` whose
            // name collides in the frame-wide `code.locals` with a *popped* sibling
            // block's slot (`{ my @a=... } { (my @a).push: $_ for ^3 }`) leaves
            // env=[] but that slot reset to Nil — env and slot disagree. Recording
            // it would wipe the accumulated `0,1,2` back to the hoisted empty value
            // at loop exit. Requiring slot==env restores only true shadows
            // (`my @a=1,2,3; for { my @a=7,8 }` keeps the outer `1,2,3`) and leaves
            // enclosing-scoped statement-modifier declarations alone.
            // Also skip names this loop already declared in a prior iteration
            // (`loop_local_vars`, populated below). After iteration 1 of an
            // accumulating statement-modifier loop, env and slot agree on the
            // partial result, which would spuriously look coherent; that value is
            // the loop's own, not an enclosing binding.
            let already_loop_local = self
                .loop_local_vars
                .last()
                .is_some_and(|s| s.contains(name));
            let has_coherent_slot = !already_loop_local
                && code
                    .locals
                    .iter()
                    .enumerate()
                    .any(|(i, n)| n.as_str() == name && self.locals.get(i) == Some(&prev));
            if has_coherent_slot
                && let Some(saved) = self.loop_local_saved_env.last_mut()
                && !saved.contains_key(name)
            {
                saved.insert(name.to_string(), prev);
            }
        }
        // Pre-initialize the variable in the env with a default value so that
        // closures created during the RHS expression can capture it.
        // This enables capture-by-reference patterns like:
        //   my $proxy := Proxy.new(STORE => -> $, \v { $proxy.VAR... })
        //
        // Skip &-sigiled variables here: seeding the lexical environment with
        // `&name = Any` before the RHS runs makes `EVAL(q[sub name() { ... }])`
        // look like a routine redeclaration instead of producing a callable to
        // bind into `my &name = ...`.
        if !name.starts_with('&') && !self.env().contains_key(name) {
            let default = if name.starts_with('@') {
                Value::real_array(Vec::new())
            } else if name.starts_with('%') {
                Value::hash(std::collections::HashMap::new())
            } else {
                Value::Package(crate::symbol::Symbol::intern("Any"))
            };
            self.env_mut().insert(name.to_string(), default);
        }
        // Track this variable as declared within the current block scope.
        // BlockScope restoration uses this to avoid propagating block-local
        // variable values back to the outer scope.
        if let Some(set) = self.block_declared_vars.last_mut() {
            set.insert(name.to_string());
        }
        // Track loop-body declarations so a closure created in the body can mark
        // this name as a per-iteration `owned_capture` (see Interpreter::loop_local_vars).
        if let Some(set) = self.loop_local_vars.last_mut() {
            set.insert(name.to_string());
        }
    }

    pub(super) fn exec_assign_expr_local_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
    ) -> Result<(), RuntimeError> {
        let r = self.exec_assign_expr_local_op_inner(code, idx);
        // Phase 3 Stage 2: write-through scalar attribute writes to the cell.
        if r.is_ok() {
            self.mirror_attr_local_to_cell(code, idx as usize);
        }
        r
    }

    fn exec_assign_expr_local_op_inner(
        &mut self,
        code: &CompiledCode,
        idx: u32,
    ) -> Result<(), RuntimeError> {
        let idx = idx as usize;
        // Slice 2a/2b: `$scalar = @arr` / chained `$r = $q` reassignment (the
        // VarDecl form goes through `exec_set_local_op`). Promote the source
        // container to a shared `ContainerRef` cell so structural mutation through
        // either name is seen by both (raku reference semantics), then leave the
        // value on the stack (assignment is an expression). No-op when the source
        // does not deref to an Array/Hash (a plain `$x = $y` stays a copy).
        let array_share_source = self.array_share_source.take();
        if self.array_share_context {
            self.array_share_context = false;
            if let Some(src) = array_share_source {
                let val = self.stack.pop().unwrap_or(Value::Nil);
                if val.with_deref(|v| matches!(v, Value::Array(..) | Value::Hash(_))) && {
                    let name = &code.locals[idx];
                    !name.starts_with('@') && !name.starts_with('%') && !name.starts_with('&')
                } {
                    self.array_share_assign(code, idx, val.clone(), src)?;
                    self.stack.push(val);
                    return Ok(());
                }
                // Not an array-share (plain scalar source / `@`/`%` target): push back.
                self.stack.push(val);
            }
        }
        // If the current local is a Proxy, invoke STORE instead of overwriting
        if let Value::Proxy { storer, .. } = &self.locals[idx]
            && !matches!(storer.as_ref(), Value::Nil)
        {
            let val = self.stack.pop().unwrap_or(Value::Nil);
            let proxy_val = self.locals[idx].clone();
            loan_env!(self, assign_proxy_lvalue(proxy_val, val))?;
            // A Proxy STORE wrote the referent caller lexical by name. For
            // substr-rw/subbuf-rw/undefine the STORE recorded the referent precisely
            // (`record_caller_var_writeback`); drain it here so the slot refreshes
            // (see the other Proxy-STORE assign site).
            self.apply_pending_rw_writeback(code);
            return Ok(());
        }

        // Fast path for simple scalar variables — skip all metadata checks
        if code.simple_locals[idx] {
            let mut val = self.stack.pop().unwrap_or(Value::Nil);
            let name = &code.locals[idx];
            // Lazy sync: if the local is not a ContainerRef but env has one
            // (from a cross-scope `:=` binding), adopt the ContainerRef and
            // write through it to preserve shared container identity.
            // Skip for type objects and complex values.
            if !self.locals[idx].is_container_ref()
                && !matches!(
                    self.locals[idx],
                    Value::Package(_)
                        | Value::Array(..)
                        | Value::Hash(..)
                        | Value::Sub(..)
                        | Value::Instance { .. }
                )
                && let Some(Value::ContainerRef(arc)) = self.env().get(name).cloned()
            {
                self.locals[idx] = Value::ContainerRef(arc);
            }
            if let Value::ContainerRef(arc) = &self.locals[idx] {
                // Slice 2a: a `=`-array-shared scalar reassigned as a whole
                // REPLACES the slot (raku value semantics); drop the share and
                // fall through to the plain-replace path below.
                let scalar = !name.starts_with('@') && !name.starts_with('%');
                if scalar && self.array_share_active && self.is_array_share_scalar(name) {
                    self.clear_array_share_marker(name);
                } else {
                    let arc = arc.clone();
                    if scalar {
                        val = Self::normalize_scalar_assignment_value(val);
                    }
                    arc.lock().unwrap().clone_from(&val);
                    self.stack.push(val);
                    self.flush_local_to_env(code, idx);
                    return Ok(());
                }
            }
            if !name.starts_with('@') && !name.starts_with('%') {
                val = Self::normalize_scalar_assignment_value(val);
            }
            if matches!(val, Value::Nil)
                && !matches!(self.locals[idx], Value::Nil)
                && let Some(def) = self.var_default(name)
            {
                val = def.clone();
            }
            if let Some(constraint) = self.var_type_constraint_fast(name).cloned() {
                let val = if matches!(val, Value::Nil) {
                    if constraint == "Mu" {
                        val
                    } else {
                        let nominal =
                            loan_env!(self, nominal_type_object_name_for_constraint(&constraint));
                        Value::Package(Symbol::intern(&nominal))
                    }
                } else if !self.type_matches_value(&constraint, &val) {
                    return Err(runtime::utils::type_check_assignment_typed_error(
                        name,
                        &constraint,
                        &val,
                    ));
                } else if !matches!(val, Value::Nil | Value::Package(_)) {
                    loan_env!(self, try_coerce_value_for_constraint(&constraint, val))?
                } else {
                    val
                };
                self.locals[idx] = val.clone();
                self.stack.push(val);
            } else {
                self.locals[idx] = val.clone();
                self.stack.push(val);
            }
            if self.fatal_mode
                && !name.contains("__mutsu_")
                && let Some(err) = self.failure_to_runtime_error_if_unhandled(&self.locals[idx])
            {
                return Err(err);
            }
            // Update env when shared_vars is active; otherwise write through to env.
            if self.shared_vars_active {
                loan_env!(self, set_shared_var(name, self.locals[idx].clone()));
            } else {
                self.flush_local_to_env(code, idx);
            }
            // Track topic mutations for map rw writeback
            if name == "_" {
                let topic = self.locals[idx].clone();
                self.env_mut()
                    .insert("__mutsu_rw_map_topic__".to_string(), topic);
            }
            self.flush_local_to_env(code, idx);
            return Ok(());
        }

        let raw_val = self.stack.pop().unwrap_or(Value::Nil);
        let name = &code.locals[idx];
        self.check_readonly_for_modify(name)?;
        let mut val = if name.starts_with('%') {
            self.coerce_hash_var_value(name, raw_val)?
        } else if name.starts_with('@') {
            let mut assigned = match raw_val {
                Value::LazyList(list) => match list.env.get(Self::LAZY_ASSIGN_PRESERVE_MARKER) {
                    Some(Value::Bool(true)) => Value::LazyList(list),
                    _ => Value::real_array(self.force_lazy_list_vm(&list)?),
                },
                other => runtime::coerce_to_array(other),
            };
            let class_name = match &self.locals[idx] {
                Value::Instance { class_name, .. } => Some(*class_name),
                Value::Package(class_name) => Some(*class_name),
                _ => None,
            };
            if let Some(class_name) = class_name {
                let class = class_name.resolve();
                if class == "Blob" || class.starts_with("blob") || class.starts_with("Blob[") {
                    return Err(RuntimeError::assignment_ro(None));
                }
                if class == "Buf" || class.starts_with("buf") || class.starts_with("Buf[") {
                    let items = runtime::value_to_list(&assigned)
                        .into_iter()
                        .map(|v| Value::Int(runtime::to_int(&v)))
                        .collect::<Vec<_>>();
                    assigned = self.try_compiled_method_or_interpret(
                        Value::Package(class_name),
                        "new",
                        items,
                    )?;
                }
            }
            assigned
        } else {
            Self::normalize_scalar_assignment_value(raw_val)
        };
        if matches!(val, Value::Nil)
            && let Some(def) = self.var_default(name)
        {
            val = def.clone();
        }
        if name.starts_with('@') || name.starts_with('%') {
            val = self.coerce_typed_container_assignment(name, val, false)?;
        }
        if let Some(constraint) = loan_env!(self, var_type_constraint(name))
            && !name.starts_with('%')
            && !name.starts_with('@')
        {
            if matches!(val, Value::Nil) {
                if constraint != "Mu" {
                    // Assigning Nil to a typed variable resets it to the type object
                    let nominal =
                        loan_env!(self, nominal_type_object_name_for_constraint(&constraint));
                    val = Value::Package(Symbol::intern(&nominal));
                }
            } else if !self.type_matches_value(&constraint, &val) {
                return Err(runtime::utils::type_check_assignment_typed_error(
                    name,
                    &constraint,
                    &val,
                ));
            }
            if !matches!(val, Value::Nil | Value::Package(_)) {
                val = loan_env!(self, try_coerce_value_for_constraint(&constraint, val))?;
            }
        }
        let readonly_key = format!("__mutsu_sigilless_readonly::{}", name);
        let alias_key = format!("__mutsu_sigilless_alias::{}", name);
        if matches!(self.env().get(&readonly_key), Some(Value::Bool(true)))
            && !matches!(self.env().get(&alias_key), Some(Value::Str(_)))
        {
            return Err(RuntimeError::assignment_ro(None));
        }
        if !name.starts_with('@') && !name.starts_with('%') && !name.starts_with('&') {
            loan_env!(self, reset_atomic_var_key(name));
        }
        if self.fatal_mode
            && !name.contains("__mutsu_")
            && let Some(err) = self.failure_to_runtime_error_if_unhandled(&val)
        {
            return Err(err);
        }
        // Apply the variable's declared element/key type to the container value
        // before it is stored, so the embedded `HashData` metadata (or array
        // side-table entry) is carried by every copy below.
        if (name.starts_with('@') || name.starts_with('%'))
            && let Some(value_type) = loan_env!(self, var_type_constraint(name))
        {
            let info = crate::runtime::ContainerTypeInfo {
                declared_type: if name.starts_with('@')
                    && crate::runtime::native_types::is_native_array_element_type(&value_type)
                {
                    Some(format!("array[{value_type}]"))
                } else {
                    None
                },
                value_type,
                key_type: if name.starts_with('%') {
                    loan_env!(self, var_hash_key_constraint(name))
                } else {
                    None
                },
            };
            val = self.tag_container_metadata(val, info);
        }
        self.locals[idx] = val.clone();
        self.set_env_with_main_alias(name, val.clone());
        if let Some(alias_name) = self.env().get(&alias_key).and_then(|v| {
            if let Value::Str(name) = v {
                Some(name.to_string())
            } else {
                None
            }
        }) {
            self.update_local_if_exists(code, &alias_name, &val);
            self.env_mut().insert(alias_name.clone(), val.clone());
            // Slice F: a sigilless param (`\target`) aliases a caller variable;
            // the env write above is the only thing that reaches it. Record the
            // alias target so the call-site drain writes it through to the
            // caller's local slot (no-op if it is not a caller local).
            self.pending_rw_writeback_sources.push(alias_name);
        }
        if let Some(attr) = name.strip_prefix('.') {
            self.env_mut().insert(format!("!{}", attr), val.clone());
        } else if let Some(attr) = name.strip_prefix('!') {
            self.env_mut().insert(format!(".{}", attr), val.clone());
        }
        self.stack.push(val);
        Ok(())
    }
}
