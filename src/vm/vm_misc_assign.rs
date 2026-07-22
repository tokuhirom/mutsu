use super::*;

impl Interpreter {
    pub(super) fn exec_assign_expr_op_inner(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        let name = match code.constants[name_idx as usize].view() {
            ValueView::Str(s) => s.to_string(),
            _ => unreachable!("AssignExpr name must be a string constant"),
        };
        self.check_readonly_for_modify(&name)?;
        if name.starts_with('%')
            && self
                .var_type_constraint_fast(&name)
                .and_then(|s| Self::quant_hash_trait_from_constraint(s.as_str()))
                == Some("Mix")
            && self
                .get_env_with_main_alias(&name)
                .is_some_and(|current| !current.is_nil())
        {
            return Err(RuntimeError::assignment_ro(None));
        }
        if name.starts_with('&') && !name.contains("::") {
            let bare = name.trim_start_matches('&');
            let has_variable_slot = self.env().contains_key(&name);
            let is_routine_symbol = self.has_function(bare)
                || self.has_multi_function(bare)
                || self.has_proto(bare)
                || self.resolve_token_defs(bare).is_some()
                || self.has_proto_token(bare);
            if is_routine_symbol && !has_variable_slot {
                return Err(RuntimeError::assignment_ro(None));
            }
        }
        // `%h = ...` where `%h` is a tied container held in env (a global, or a
        // captured lexical reached through this env-named path rather than a
        // local slot) must route through the class's STORE, exactly like the
        // local-slot `maybe_tied_store_reassign` does.
        if self.maybe_tied_store_reassign_named(&name)?.is_some() {
            return Ok(());
        }
        let raw_val = self.stack.pop().unwrap_or(Value::NIL);
        let (raw_val, bind_source) = match raw_val.view() {
            ValueView::VarRef { name, value, .. } => (value.clone(), Some(name.resolve())),
            _ => (raw_val, None),
        };
        // Capture old hash Arc pointer for circular reference fixup, and the
        // backing `Gc` for the in-place whole-container reassignment below. A
        // celled `@`/`%` aggregate (a `state @foo` / captured boxed container)
        // holds a `ContainerRef` — capture the cell so the store below goes
        // THROUGH it (identity + copy semantics) instead of clobbering it.
        let mut inplace_container_cell: Option<crate::gc::Gc<std::sync::Mutex<Value>>> = None;
        let mut inplace_old_hash: Option<crate::gc::Gc<crate::value::HashData>> = None;
        let old_hash_arc = if name.starts_with('%') {
            let current = self.get_env_with_main_alias(&name).or_else(|| {
                code.locals.iter().position(|n| n == &name).and_then(|idx| {
                    if idx < self.locals.len() {
                        Some(self.locals[idx].clone())
                    } else {
                        None
                    }
                })
            });
            match current.as_ref().map(Value::view) {
                Some(ValueView::Hash(arc)) => {
                    if !name.contains("__ANON") {
                        inplace_old_hash = Some(arc.clone());
                    }
                    Some(crate::gc::Gc::as_ptr(&arc) as usize)
                }
                Some(ValueView::ContainerRef(cell)) => {
                    inplace_container_cell = Some(cell.clone());
                    None
                }
                _ => None,
            }
        } else {
            None
        };
        // Capture old array Arc pointer for circular reference fixup, and the
        // backing `Gc` for the in-place whole-container reassignment below.
        let mut inplace_old_array: Option<crate::gc::Gc<crate::value::ArrayData>> = None;
        let old_array_arc = if name.starts_with('@') {
            let current = self.get_env_with_main_alias(&name).or_else(|| {
                code.locals.iter().position(|n| n == &name).and_then(|idx| {
                    if idx < self.locals.len() {
                        Some(self.locals[idx].clone())
                    } else {
                        None
                    }
                })
            });
            match current.as_ref().map(Value::view) {
                Some(ValueView::Array(arc, _)) => {
                    if !name.contains("__ANON") {
                        inplace_old_array = Some(arc.clone());
                    }
                    Some(crate::gc::Gc::as_ptr(&arc) as usize)
                }
                Some(ValueView::ContainerRef(cell)) => {
                    inplace_container_cell = Some(cell.clone());
                    None
                }
                _ => None,
            }
        } else {
            None
        };
        let mut val = if name.starts_with('%') {
            let hash_val = self.coerce_object_to_hash(raw_val);
            // Resolve hash sentinel entries (bound variable refs) when assigning
            // to a new hash variable. Assignment creates new containers, so bound
            // refs must be resolved to their current values.
            if let ValueView::Hash(items) = hash_val.view() {
                if Self::hash_has_sentinels(&items) {
                    self.resolve_hash_for_iteration(&items)
                } else {
                    hash_val
                }
            } else {
                hash_val
            }
        } else if name.starts_with('@') {
            let mut assigned = runtime::coerce_to_array(raw_val);
            // Check for shaped array on current value, and preserve on re-assignment.
            // The local slot and the env copy of a variable can diverge (the
            // `env_dirty` dual store): a method call between two assignments may
            // flush an unshaped copy into one store while the other still holds
            // the shaped array. A shaped array's shape is authoritative, so prefer
            // whichever store currently presents a shaped value; otherwise fall
            // back to the local slot (then env). Without this, an `@arr = (...)`
            // used as an expression (`is (@arr = ()), ...`) reads the stale
            // unshaped local and silently shrinks a fixed-dimension array.
            let local_val = code
                .locals
                .iter()
                .position(|n| n == &name)
                .and_then(|idx| self.locals.get(idx).cloned());
            let env_val = self.get_env_with_main_alias(&name);
            let is_shaped = |v: &Value| crate::runtime::utils::shaped_array_shape(v).is_some();
            let current_val = match (&local_val, &env_val) {
                (Some(l), _) if is_shaped(l) => local_val.clone(),
                (_, Some(e)) if is_shaped(e) => env_val.clone(),
                (Some(_), _) => local_val.clone(),
                _ => env_val.clone(),
            };
            let current_shape = current_val
                .as_ref()
                .and_then(crate::runtime::utils::shaped_array_shape)
                .or_else(|| {
                    // Also check declared shape metadata for multi-dim
                    let key = format!("__mutsu_shaped_array_dims::{}", name);
                    self.env().get(&key).and_then(|v| {
                        if let ValueView::Array(dims, ..) = v.view() {
                            Some(
                                dims.iter()
                                    .filter_map(|d| {
                                        if let ValueView::Int(n) = d.view() {
                                            Some(n as usize)
                                        } else {
                                            None
                                        }
                                    })
                                    .collect(),
                            )
                        } else {
                            None
                        }
                    })
                });
            // Only preserve the old shape if the new value is NOT already shaped
            let assigned_has_own_shape = crate::runtime::utils::shaped_array_shape(&assigned)
                .is_some()
                || matches!(
                    assigned.view(),
                    ValueView::Array(_, crate::value::ArrayKind::Shaped)
                );
            if let Some(ref shape) = current_shape {
                // For 1D shaped arrays, rebuild with the same shape
                if shape.len() == 1 && !assigned_has_own_shape {
                    let items = runtime::value_to_list(&assigned);
                    let item_count = items.len();
                    let mut shaped_items: Vec<Value> = items.into_iter().take(shape[0]).collect();
                    if item_count < shape[0] {
                        shaped_items.resize(shape[0], Value::NIL);
                    }
                    assigned = Value::array_with_kind(
                        crate::gc::Gc::new(crate::value::ArrayData::new(shaped_items)),
                        crate::value::ArrayKind::Shaped,
                    );
                    crate::runtime::utils::mark_shaped_array(&assigned, Some(shape));
                    // Preserve container type metadata from old array
                    if let Some(ref cv) = current_val
                        && let Some(info) = self.container_type_metadata(cv)
                    {
                        assigned = self.tag_container_metadata(assigned, info);
                    }
                }
            }
            if let Some(current) = self.get_env_with_main_alias(&name) {
                let class_name = match current.view() {
                    ValueView::Instance { class_name, .. } => Some(class_name),
                    ValueView::Package(class_name) => Some(class_name),
                    _ => None,
                };
                if let Some(class_name) = class_name {
                    let class = class_name.resolve();
                    if class == "Blob" || class.starts_with("blob") {
                        return Err(RuntimeError::assignment_ro(None));
                    }
                    if class == "Buf" || class.starts_with("buf") {
                        let items = runtime::value_to_list(&assigned)
                            .into_iter()
                            .map(|v| Value::int(runtime::to_int(&v)))
                            .collect::<Vec<_>>();
                        assigned = self.try_compiled_method_or_interpret(
                            Value::package(class_name),
                            "new",
                            items,
                        )?;
                    }
                }
            }
            assigned
        } else {
            Self::itemize_scalar_store(&name, Self::normalize_scalar_assignment_value(raw_val))
        };
        if val.is_nil()
            && let Some(def) = self.var_default(&name)
        {
            val = def.clone();
        }
        if self.fatal_mode
            && !name.contains("__mutsu_")
            && let Some(err) = self.failure_to_runtime_error_if_unhandled(&val)
        {
            return Err(err);
        }
        // Validate/coerce against a scalar type constraint. Nil resets to the
        // type object; a non-Nil value that violates the constraint throws
        // X::TypeCheck::Assignment (e.g. `infix:<=>($int, "foo")`), matching the
        // SetLocal path -- expression-context assignment to a captured typed
        // scalar must type-check just like a statement-level one. An untyped
        // scalar resets Nil to the default type object Any.
        let mut val = if !name.starts_with('@')
            && !name.starts_with('%')
            && let Some(constraint) = loan_env!(self, var_type_constraint(&name))
        {
            if val.is_nil() {
                if constraint == "Mu" {
                    val
                } else {
                    let nominal =
                        loan_env!(self, nominal_type_object_name_for_constraint(&constraint));
                    Value::package(Symbol::intern(&nominal))
                }
            } else if !self.type_matches_value(&constraint, &val) {
                return Err(runtime::utils::type_check_assignment_typed_error(
                    &name,
                    &constraint,
                    &val,
                ));
            } else if !matches!(val.view(), ValueView::Package(_)) {
                loan_env!(self, try_coerce_value_for_constraint(&constraint, val))?
            } else {
                val
            }
        } else {
            // Untyped scalar: Nil resets to the default type object Any (a no-op
            // for `@`/`%` containers).
            self.reset_nil_untyped_scalar(&name, val)
        };
        let readonly_key = format!("__mutsu_sigilless_readonly::{}", name);
        let alias_key = format!("__mutsu_sigilless_alias::{}", name);
        if matches!(
            self.env().get(&readonly_key).map(Value::view),
            Some(ValueView::Bool(true))
        ) && !matches!(
            self.env().get(&alias_key).map(Value::view),
            Some(ValueView::Str(_))
        ) {
            return Err(RuntimeError::assignment_ro(None));
        }
        if let Some(source_name) = bind_source {
            let mut resolved_source = source_name;
            let mut seen = std::collections::HashSet::new();
            while seen.insert(resolved_source.clone()) {
                let key = format!("__mutsu_sigilless_alias::{}", resolved_source);
                let Some(ValueView::Str(next)) = self.env().get(&key).map(Value::view) else {
                    break;
                };
                resolved_source = next.to_string();
            }
            self.env_mut()
                .insert(alias_key.clone(), Value::str(resolved_source));
            self.env_mut().insert(readonly_key, Value::FALSE);
            self.mark_sigilless_alias_seen();
        }
        // If the current value is a Proxy (in locals or env), invoke STORE instead of overwriting
        {
            let current_proxy = code
                .locals
                .iter()
                .position(|n| n == &name)
                .and_then(|idx| {
                    if idx < self.locals.len() {
                        let v = &self.locals[idx];
                        if matches!(v.view(), ValueView::Proxy { .. }) {
                            Some(v.clone())
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                })
                .or_else(|| {
                    self.get_env_with_main_alias(&name).and_then(|v| {
                        if matches!(v.view(), ValueView::Proxy { .. }) {
                            Some(v)
                        } else {
                            None
                        }
                    })
                });
            if let Some(ValueView::Proxy { storer, .. }) = current_proxy.as_ref().map(Value::view)
                && !storer.is_nil()
            {
                let proxy_val = current_proxy.unwrap();
                loan_env!(self, assign_proxy_lvalue(proxy_val, val.clone()))?;
                // A Proxy STORE (e.g. `$r := substr-rw($str, ...); $r = v`) mutates
                // the referent caller lexical (`$str`) by name in env. The STORE
                // records the referent on the retain-on-miss writeback list
                // (`record_caller_var_writeback`); drain it here so the owner's slot
                // is refreshed precisely without the blanket env→locals pull
                // (substrate step toward env_dirty removal; byte-identical under ON).
                self.apply_pending_rw_writeback(code);
                self.stack.push(val);
                return Ok(());
            }
        }
        // Write through a `ContainerRef` slot/binding (e.g. a raw `\target` bound
        // to a multi-dim subscript lvalue, or a `:=`-bound scalar reached by name)
        // so an expression-context assignment mutates the shared cell instead of
        // detaching the alias. Mirrors the SetLocal / AssignExprLocal write-through.
        {
            let current = code
                .locals
                .iter()
                .position(|n| n == &name)
                .and_then(|idx| self.locals.get(idx).cloned())
                .or_else(|| self.get_env_with_main_alias(&name));
            if let Some(ValueView::ContainerRef(arc)) = current.as_ref().map(Value::view) {
                // Restrict to scalar (sigilless `\target` / `$`) names: `@`/`%`
                // vars have their own ContainerRef handling and must keep their
                // existing whole-reassignment semantics here.
                let scalar = !name.starts_with('@') && !name.starts_with('%');
                if scalar && !(self.array_share_active && self.is_array_share_scalar(&name)) {
                    let arc = arc.clone();
                    self.check_container_cell_constraint(&arc, &val)?;
                    Value::store_through_cell(&arc, &val);
                    self.stack.push(val);
                    return Ok(());
                }
            }
            // First write through a missing-key bind reached by name (a
            // `HashEntryRef` deferred token, e.g. a `\target` bound to
            // `%h{$a;$b;$c}` written inside a closure that captured `target`,
            // like a `subtest { ... }` block): materialize the path into a
            // shared `ContainerRef` cell. The name-based counterpart of the
            // SetLocal / AssignExprLocal materialization.
            if matches!(
                current.as_ref().map(Value::view),
                Some(ValueView::HashEntryRef { .. })
            ) && !name.starts_with('@')
                && !name.starts_with('%')
                && let Some(token) = current.as_ref()
                && let Some((arc, key)) = token.hash_entry_terminal()
            {
                let cell = crate::gc::Gc::new(std::sync::Mutex::new(val.clone()));
                // SAFETY: aliased in-place mutation of a shared hash; see
                // `arc_contents_mut`. No live borrow into the map.
                let hd = unsafe { crate::value::gc_contents_mut(&arc) };
                Value::hash_insert_through(&mut hd.map, key, Value::container_ref(cell.clone()));
                let cell_val = Value::container_ref(cell);
                if let Some(idx) = code.locals.iter().position(|n| n == &name)
                    && idx < self.locals.len()
                {
                    self.locals[idx] = cell_val.clone();
                }
                self.set_env_with_main_alias(&name, cell_val);
                self.stack.push(val);
                return Ok(());
            }
            // A sigilless `\target` bound to a multi-dim slice lvalue distributes
            // the RHS element-wise through its cells (the env-named counterpart
            // of the `exec_assign_expr_local_op_inner` write-through — reached
            // when the assignment happens inside a closure that captured
            // `target`, e.g. a `subtest { ... }` block).
            if let Some(holder) = current.clone()
                && let Some(res) = self.distribute_bound_multidim_slice(&name, &holder, &val)
            {
                res?;
                self.stack.push(val);
                return Ok(());
            }
        }
        // Circular hash reference fixup
        if name.starts_with('%') {
            Self::fixup_circular_hash_refs(&mut val, &old_hash_arc);
            // Also update the Dup'd copy on the stack (if any) so that the
            // expression result also has the circular reference.
            if let Some(old_ptr) = old_hash_arc
                && let Some(stack_top) = self.stack.last_mut()
                && let ValueView::Hash(stack_arc) = stack_top.view()
            {
                let has_old_ref = stack_arc.values().any(|v| {
                    if let ValueView::Hash(inner_arc) = v.view() {
                        crate::gc::Gc::as_ptr(&inner_arc) as usize == old_ptr
                    } else {
                        false
                    }
                });
                if has_old_ref {
                    *stack_top = val.clone();
                }
            }
        }
        // Circular array reference fixup
        if name.starts_with('@') {
            Self::fixup_circular_array_refs(&mut val, &old_array_arc);
            if let Some(old_ptr) = old_array_arc
                && let Some(stack_top) = self.stack.last_mut()
                && let ValueView::Array(stack_arc, _) = stack_top.view()
            {
                let has_old_ref = stack_arc.iter().any(|v| {
                    if let ValueView::Array(inner_arc, _) = v.view() {
                        crate::gc::Gc::as_ptr(&inner_arc) as usize == old_ptr
                    } else {
                        false
                    }
                });
                if has_old_ref {
                    *stack_top = val.clone();
                }
            }
        }
        // Container identity (§3, splice.t): a plain whole-hash/array
        // reassignment reached by NAME (e.g. a captured outer `%h = ...` inside
        // a sub) mutates the EXISTING container in place, mirroring the SetLocal
        // slot path, so every other holder of the same backing `Gc` (a by-value
        // capture in a list, the caller's local slot) observes the update.
        if let Some(old_gc) = &inplace_old_hash
            && let ValueView::Hash(new_gc) = val.view()
            && !crate::gc::Gc::ptr_eq(old_gc, &new_gc)
        {
            let new_gc = new_gc.clone();
            val = Self::hash_inplace_reassign(old_gc, &new_gc);
        } else if let Some(old_gc) = &inplace_old_array
            && let ValueView::Array(new_gc, kind) = val.view()
            && !crate::gc::Gc::ptr_eq(old_gc, &new_gc)
        {
            let (new_gc, kind) = (new_gc.clone(), kind);
            val = Self::array_inplace_reassign(old_gc, &new_gc, kind);
        } else if let Some(cell) = &inplace_container_cell {
            // A celled `@`/`%` aggregate (`(state @foo) = @bar`): store through
            // the cell with the inner container's identity preserved (contents
            // copied — `=` copy semantics — so a later `@foo[0]++` cannot reach
            // the RHS source array), and keep the CELL installed in env/locals.
            Self::cell_store_preserving_container_identity(cell, &val);
            let cell_val = Value::container_ref(cell.clone());
            self.update_local_if_exists(code, &name, &cell_val);
            self.set_env_with_main_alias(&name, cell_val);
            self.sync_anon_state_value(&name, &val);
            self.stack
                .push(Self::itemize_scalar_assign_result(&name, val));
            return Ok(());
        }
        self.update_local_if_exists(code, &name, &val);
        self.set_env_with_main_alias(&name, val.clone());
        // Persist anonymous state variable (`$`) across closure calls.
        self.sync_anon_state_value(&name, &val);
        // Track topic mutations for map rw writeback: when `$_` (= "_") is
        // explicitly assigned, record the value so `eval_map_over_items_rw` can
        // read it back even after the block return value overwrites `_`.
        if name == "_" {
            self.env_mut()
                .insert("__mutsu_rw_map_topic__".to_string(), val.clone());
        }
        let mut alias_name = self.env().get(&alias_key).and_then(|v| {
            if let ValueView::Str(name) = v.view() {
                Some(name.to_string())
            } else {
                None
            }
        });
        let mut seen_aliases = std::collections::HashSet::new();
        while let Some(current_alias) = alias_name {
            if !seen_aliases.insert(current_alias.clone()) {
                break;
            }
            self.update_local_if_exists(code, &current_alias, &val);
            self.env_mut().insert(current_alias.clone(), val.clone());
            let next_key = format!("__mutsu_sigilless_alias::{}", current_alias);
            alias_name = self.env().get(&next_key).and_then(|v| {
                if let ValueView::Str(name) = v.view() {
                    Some(name.to_string())
                } else {
                    None
                }
            });
        }
        if let Some(attr) = name.strip_prefix('.') {
            self.env_mut().insert(format!("!{}", attr), val.clone());
        } else if let Some(attr) = name.strip_prefix('!') {
            self.env_mut().insert(format!(".{}", attr), val.clone());
        }
        if name == "_"
            && let Some(ref source_var) = self.topic_source_var
            && !source_var.starts_with('@')
            && !source_var.starts_with('%')
        {
            let sv = source_var.clone();
            self.set_env_with_main_alias(&sv, val.clone());
            self.update_local_if_exists(code, &sv, &val);
        }
        // A scalar assignment used as an rvalue yields the *itemized* container
        // value, so a following list context treats it as one element (matching
        // the local-var path). Covers package-qualified / global scalars
        // (`@z = ($Foo::c = 3, 4)` -> `((3,4),)`). `@`/`%`/`&` keep their value.
        self.stack
            .push(Self::itemize_scalar_assign_result(&name, val));
        Ok(())
    }

    /// Tag the top-of-stack value with the name of the variable it was read
    /// from, so the binder can alias the caller's container for an `is rw` /
    /// `is raw` / `:=` target (see [`Value::varref`]).
    ///
    /// This runs once per variable passed as an argument — the single hottest
    /// opcode in a call-heavy program (99.7% of `bench-tak`'s interpreted ops),
    /// so it must not allocate more than the one `VarRef` payload. The name is
    /// taken from the chunk's pre-interned constant symbols: no `String`, no
    /// hashing.
    pub(super) fn exec_wrap_var_ref_op(&mut self, code: &CompiledCode, name_idx: u32) {
        let value = self.stack.pop().unwrap_or(Value::NIL);
        self.stack
            .push(Value::varref(code.const_sym(name_idx), value, None));
    }

    /// Validate and coerce a value for native integer type assignment.
    /// Throws on: string values, non-integer numerics (floats), NaN, out-of-range values.
    pub(super) fn validate_native_int_assignment(
        &mut self,
        type_name: &str,
        value: &Value,
    ) -> Result<(), RuntimeError> {
        use crate::runtime::native_types;
        use num_bigint::BigInt as NumBigInt;
        use num_traits::ToPrimitive;

        // Reject strings
        if matches!(value.view(), ValueView::Str(_)) {
            return Err(RuntimeError::new(format!(
                "Cannot convert string to native integer type '{}'",
                type_name
            )));
        }
        // Reject NaN
        if let ValueView::Num(n) = value.view() {
            if n.is_nan() {
                return Err(RuntimeError::new(format!(
                    "Cannot convert NaN to native integer type '{}'",
                    type_name
                )));
            }
            // Reject non-integer floats
            if n.fract() != 0.0 {
                return Err(RuntimeError::new(format!(
                    "Cannot convert non-integer value to native integer type '{}'",
                    type_name
                )));
            }
        }
        // Reject Rat with non-integer value
        if let ValueView::Rat(n, d) = value.view()
            && d != 0
            && n % d != 0
        {
            return Err(RuntimeError::new(format!(
                "Cannot convert non-integer value to native integer type '{}'",
                type_name
            )));
        }

        // Convert value to BigInt for range checking
        let big_val = match value.view() {
            ValueView::Int(n) => NumBigInt::from(n),
            ValueView::BigInt(n) => (**n).clone(),
            ValueView::Num(n) => NumBigInt::from(n as i64),
            ValueView::Rat(n, d) => {
                if d == 0 {
                    NumBigInt::from(0)
                } else {
                    NumBigInt::from(n / d)
                }
            }
            ValueView::Bool(b) => NumBigInt::from(if b { 1 } else { 0 }),
            _ => {
                return Err(RuntimeError::new(format!(
                    "Cannot convert value to native integer type '{}'",
                    type_name
                )));
            }
        };

        // Wrap out-of-range values for smaller native types (like C semantics).
        // Full-width signed types (int/int64) throw on overflow.
        // Unsigned types (uint/uint64) wrap negative values to unsigned range.
        let wrapped = if !native_types::is_in_native_range(type_name, &big_val) {
            if matches!(type_name, "int" | "int64") {
                return Err(RuntimeError::new(format!(
                    "Cannot unbox {} bit wide bigint into native integer",
                    big_val.bits()
                )));
            }
            if matches!(type_name, "uint" | "uint64") {
                // Positive values exceeding u64 range should throw
                if big_val > NumBigInt::from(0u64) {
                    return Err(RuntimeError::new(format!(
                        "Cannot unbox {} bit wide bigint into native integer",
                        big_val.bits()
                    )));
                }
            }
            native_types::wrap_native_int(type_name, &big_val)
        } else {
            big_val
        };

        // Coerce to Int on the stack
        let int_val = wrapped.to_i64().map(Value::int).unwrap_or_else(|| {
            // For uint64 values that don't fit in i64, store as BigInt
            Value::bigint(wrapped)
        });
        *self.stack.last_mut().unwrap() = int_val;
        Ok(())
    }
}
