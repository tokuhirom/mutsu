use super::*;

impl Interpreter {
    pub(super) fn exec_assign_expr_op_inner(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        let name = match &code.constants[name_idx as usize] {
            Value::Str(s) => s.to_string(),
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
                .is_some_and(|current| !matches!(current, Value::Nil))
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
        let raw_val = self.stack.pop().unwrap_or(Value::Nil);
        let (raw_val, bind_source) = if let Value::Capture { positional, named } = &raw_val {
            if positional.is_empty() {
                if let (Some(Value::Str(source_name)), Some(inner)) = (
                    named.get("__mutsu_varref_name"),
                    named.get("__mutsu_varref_value"),
                ) {
                    (inner.clone(), Some(source_name.to_string()))
                } else {
                    (raw_val, None)
                }
            } else {
                (raw_val, None)
            }
        } else {
            (raw_val, None)
        };
        // Capture old hash Arc pointer for circular reference fixup.
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
            if let Some(Value::Hash(arc)) = current {
                Some(Arc::as_ptr(&arc) as usize)
            } else {
                None
            }
        } else {
            None
        };
        // Capture old array Arc pointer for circular reference fixup.
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
            if let Some(Value::Array(arc, _)) = current {
                Some(Arc::as_ptr(&arc) as usize)
            } else {
                None
            }
        } else {
            None
        };
        let mut val = if name.starts_with('%') {
            let hash_val = runtime::coerce_to_hash(raw_val);
            // Resolve hash sentinel entries (bound variable refs) when assigning
            // to a new hash variable. Assignment creates new containers, so bound
            // refs must be resolved to their current values.
            if let Value::Hash(ref items) = hash_val {
                if Self::hash_has_sentinels(items) {
                    self.resolve_hash_for_iteration(items)
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
                        if let Value::Array(dims, ..) = v {
                            Some(
                                dims.iter()
                                    .filter_map(|d| {
                                        if let Value::Int(n) = d {
                                            Some(*n as usize)
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
                || matches!(&assigned, Value::Array(_, crate::value::ArrayKind::Shaped));
            if let Some(ref shape) = current_shape {
                // For 1D shaped arrays, rebuild with the same shape
                if shape.len() == 1 && !assigned_has_own_shape {
                    let items = runtime::value_to_list(&assigned);
                    let item_count = items.len();
                    let mut shaped_items: Vec<Value> = items.into_iter().take(shape[0]).collect();
                    if item_count < shape[0] {
                        shaped_items.resize(shape[0], Value::Nil);
                    }
                    assigned = Value::Array(
                        std::sync::Arc::new(crate::value::ArrayData::new(shaped_items)),
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
                let class_name = match &current {
                    Value::Instance { class_name, .. } => Some(*class_name),
                    Value::Package(class_name) => Some(*class_name),
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
                            .map(|v| Value::Int(runtime::to_int(&v)))
                            .collect::<Vec<_>>();
                        assigned = self.try_compiled_method_or_interpret(
                            Value::Package(class_name),
                            "new",
                            items,
                        )?;
                    }
                }
            }
            assigned
        } else {
            Self::normalize_scalar_assignment_value(raw_val)
        };
        if matches!(val, Value::Nil)
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
        // When assigning Nil to a typed variable, reset to the type object
        let mut val =
            if matches!(val, Value::Nil) && !name.starts_with('@') && !name.starts_with('%') {
                if let Some(constraint) = loan_env!(self, var_type_constraint(&name)) {
                    if constraint == "Mu" {
                        val
                    } else {
                        let nominal =
                            loan_env!(self, nominal_type_object_name_for_constraint(&constraint));
                        Value::Package(Symbol::intern(&nominal))
                    }
                } else {
                    val
                }
            } else {
                val
            };
        let readonly_key = format!("__mutsu_sigilless_readonly::{}", name);
        let alias_key = format!("__mutsu_sigilless_alias::{}", name);
        if matches!(self.env().get(&readonly_key), Some(Value::Bool(true)))
            && !matches!(self.env().get(&alias_key), Some(Value::Str(_)))
        {
            return Err(RuntimeError::assignment_ro(None));
        }
        if let Some(source_name) = bind_source {
            let mut resolved_source = source_name;
            let mut seen = std::collections::HashSet::new();
            while seen.insert(resolved_source.clone()) {
                let key = format!("__mutsu_sigilless_alias::{}", resolved_source);
                let Some(Value::Str(next)) = self.env().get(&key) else {
                    break;
                };
                resolved_source = next.to_string();
            }
            self.env_mut()
                .insert(alias_key.clone(), Value::str(resolved_source));
            self.env_mut().insert(readonly_key, Value::Bool(false));
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
                        if matches!(v, Value::Proxy { .. }) {
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
                        if matches!(&v, Value::Proxy { .. }) {
                            Some(v)
                        } else {
                            None
                        }
                    })
                });
            if let Some(Value::Proxy { ref storer, .. }) = current_proxy
                && !matches!(storer.as_ref(), Value::Nil)
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
            if let Some(Value::ContainerRef(arc)) = current {
                // Restrict to scalar (sigilless `\target` / `$`) names: `@`/`%`
                // vars have their own ContainerRef handling and must keep their
                // existing whole-reassignment semantics here.
                let scalar = !name.starts_with('@') && !name.starts_with('%');
                if scalar && !(self.array_share_active && self.is_array_share_scalar(&name)) {
                    arc.lock().unwrap().clone_from(&val);
                    self.stack.push(val);
                    return Ok(());
                }
            }
        }
        // Circular hash reference fixup
        if name.starts_with('%') {
            Self::fixup_circular_hash_refs(&mut val, &old_hash_arc);
            // Also update the Dup'd copy on the stack (if any) so that the
            // expression result also has the circular reference.
            if let Some(old_ptr) = old_hash_arc
                && let Some(stack_top) = self.stack.last_mut()
                && let Value::Hash(stack_arc) = stack_top
            {
                let has_old_ref = stack_arc.values().any(|v| {
                    if let Value::Hash(inner_arc) = v {
                        Arc::as_ptr(inner_arc) as usize == old_ptr
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
                && let Value::Array(stack_arc, _) = stack_top
            {
                let has_old_ref = stack_arc.iter().any(|v| {
                    if let Value::Array(inner_arc, _) = v {
                        Arc::as_ptr(inner_arc) as usize == old_ptr
                    } else {
                        false
                    }
                });
                if has_old_ref {
                    *stack_top = val.clone();
                }
            }
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
            if let Value::Str(name) = v {
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
                if let Value::Str(name) = v {
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
        self.stack.push(val);
        Ok(())
    }

    pub(super) fn exec_wrap_var_ref_op(&mut self, code: &CompiledCode, name_idx: u32) {
        let value = self.stack.pop().unwrap_or(Value::Nil);
        let name = Self::const_str(code, name_idx).to_string();
        let mut named = std::collections::HashMap::new();
        named.insert("__mutsu_varref_name".to_string(), Value::str(name));
        named.insert("__mutsu_varref_value".to_string(), value);
        self.stack.push(Value::capture(Vec::new(), named));
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
        if matches!(value, Value::Str(_)) {
            return Err(RuntimeError::new(format!(
                "Cannot convert string to native integer type '{}'",
                type_name
            )));
        }
        // Reject NaN
        if let Value::Num(n) = value {
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
        if let Value::Rat(n, d) = value
            && *d != 0
            && *n % *d != 0
        {
            return Err(RuntimeError::new(format!(
                "Cannot convert non-integer value to native integer type '{}'",
                type_name
            )));
        }

        // Convert value to BigInt for range checking
        let big_val = match value {
            Value::Int(n) => NumBigInt::from(*n),
            Value::BigInt(n) => (**n).clone(),
            Value::Num(n) => NumBigInt::from(*n as i64),
            Value::Rat(n, d) => {
                if *d == 0 {
                    NumBigInt::from(0)
                } else {
                    NumBigInt::from(*n / *d)
                }
            }
            Value::Bool(b) => NumBigInt::from(if *b { 1 } else { 0 }),
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
        let int_val = wrapped.to_i64().map(Value::Int).unwrap_or_else(|| {
            // For uint64 values that don't fit in i64, store as BigInt
            Value::bigint(wrapped)
        });
        *self.stack.last_mut().unwrap() = int_val;
        Ok(())
    }
}
