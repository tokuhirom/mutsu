use super::*;
use std::collections::HashMap;
use std::sync::Arc;

impl Interpreter {
    /// Apply a fused compound-assignment base operator to `left OP right` by
    /// reusing the exact `exec_*_op` the plain `Binary` path runs (so operator
    /// semantics, coercions, and user-`infix` overloads are identical). The
    /// operands are pushed left-then-right (matching the stack order each
    /// `exec_*_op` pops) and the result is popped back off.
    pub(crate) fn apply_compound_base_op(
        &mut self,
        op: crate::opcode::CompoundBaseOp,
        left: Value,
        right: Value,
    ) -> Result<Value, RuntimeError> {
        use crate::opcode::CompoundBaseOp as B;
        self.stack.push(left);
        self.stack.push(right);
        match op {
            B::Add => self.exec_add_op()?,
            B::Sub => self.exec_sub_op()?,
            B::Mul => self.exec_mul_op()?,
            B::Div => self.exec_div_op()?,
            B::Mod => self.exec_mod_op()?,
            B::Pow => self.exec_pow_op()?,
            B::Concat => self.exec_concat_op()?,
            B::BitAnd => self.exec_bit_and_op(),
            B::BitOr => self.exec_bit_or_op(),
            B::BitXor => self.exec_bit_xor_op(),
            B::BitShiftLeft => self.exec_bit_shift_left_op(),
            B::BitShiftRight => self.exec_bit_shift_right_op(),
            B::IntDiv => self.exec_int_div_op()?,
            B::IntMod => self.exec_int_mod_op()?,
            B::Gcd => self.exec_gcd_op(),
            B::Lcm => self.exec_lcm_op(),
            B::InfixMin => self.exec_infix_min_op(),
            B::InfixMax => self.exec_infix_max_op(),
            B::StringRepeat => self.exec_string_repeat_op()?,
        }
        Ok(self.stack.pop().unwrap())
    }

    /// Execute a fused compound assignment to a NAMED (env) scalar: `$x OP= rhs`.
    /// The rhs is already on the stack. Structurally mirrors
    /// `exec_post_increment_op_inner`: a `ContainerRef` cell gets an atomic
    /// locked read-modify-write (Track C cross-thread atomicity), a `Proxy` is
    /// fetched/op'd/stored, and a plain env scalar is written through the shared
    /// `store_named_scalar_rmw_result` tail so propagation is identical to `++`.
    /// Leaves the new value on the stack.
    pub(super) fn exec_atomic_compound_var_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        op: crate::opcode::CompoundBaseOp,
    ) -> Result<(), RuntimeError> {
        let rhs = self.stack.pop().unwrap();
        self.resolve_pending_alias_binds(code);
        let name = Self::const_str(code, name_idx);
        self.check_readonly_for_increment(name)?;
        // Default to Nil (NOT Int(0) like `++`) so `my $w; $w ~= "z"` yields "z",
        // not "0z"; the binary op descalarizes/numifies/stringifies Nil itself.
        let raw_val = self
            // A package-scope free variable (`our $X` / `package { my $X }`)
            // reached by bare name from inside a named sub is not in the local
            // env; read it from the canonical package store so the fused RMW
            // operates on the current value, not Nil (mirrors `GetGlobal`). A
            // boxed lexical's cell is authoritative and must win over a stale
            // plain `env` copy left by a prior call's return-merge.
            .package_scope_lexical(name)
            .or_else(|| self.get_env_with_main_alias(name))
            .or_else(|| self.read_package_scope_var(name))
            .or_else(|| self.anon_state_value(name))
            .unwrap_or(Value::Nil);
        // ContainerRef cell: atomic RMW under the cell lock so concurrent
        // `start { $shared += n }` blocks don't lose updates (Track C).
        if let Value::ContainerRef(ref arc) = raw_val {
            let arc = arc.clone();
            // Hold the cell lock across the whole read-modify-write so concurrent
            // threads can't interleave and lose updates. `rhs` was already popped
            // (a concrete value), and the base op operates only on `old`/`rhs`, so
            // it never re-enters this cell — no deadlock.
            let mut guard = arc.lock().unwrap();
            let old = guard.clone();
            let new_val = self.apply_compound_base_op(op, old, rhs)?;
            *guard = new_val.clone();
            drop(guard);
            self.stack.push(new_val);
            return Ok(());
        }
        // Proxy: FETCH -> op -> STORE.
        if let Value::Proxy { storer, .. } = &raw_val
            && !matches!(storer.as_ref(), Value::Nil)
        {
            let fetched = loan_env!(self, auto_fetch_proxy(&raw_val))?;
            let new_val = self.apply_compound_base_op(op, fetched, rhs)?;
            loan_env!(self, assign_proxy_lvalue(raw_val, new_val.clone()))?;
            self.stack.push(new_val);
            return Ok(());
        }
        // Plain env scalar: compute old OP rhs, store via the shared `++` tail so
        // METHOD captured-outer propagation is identical to `++` by construction.
        let new_val = self.apply_compound_base_op(op, raw_val, rhs)?;
        let stored = self.store_named_scalar_rmw_result(code, name, new_val)?;
        self.stack.push(stored);
        Ok(())
    }

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

    pub(crate) fn exec_post_increment_op_inner(
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
            .package_scope_lexical(name)
            .or_else(|| self.get_env_with_main_alias(name))
            .or_else(|| self.read_package_scope_var(name))
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

    pub(crate) fn exec_post_decrement_op_inner(
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
            .package_scope_lexical(name)
            .or_else(|| self.get_env_with_main_alias(name))
            .or_else(|| self.read_package_scope_var(name))
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

    pub(crate) fn exec_inc_dec_index_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        increment: bool,
        return_new: bool,
    ) -> Result<(), RuntimeError> {
        let name = Self::const_str(code, name_idx).to_string();
        // Element type constraint of the variable, used to fill array holes with
        // the proper type object (`(Int)`) instead of Nil when autovivifying.
        let declared_constraint_incdec = loan_env!(self, var_type_constraint(&name));
        let declared_type_incdec = self
            .env()
            .get(&name)
            .cloned()
            .and_then(|v| self.container_type_metadata(&v))
            .and_then(|info| info.declared_type);
        let _target_is_mixhash_incdec = declared_type_incdec
            .as_deref()
            .is_some_and(|t| t == "MixHash");
        let _target_is_baghash_incdec = declared_type_incdec
            .as_deref()
            .is_some_and(|t| t == "BagHash");
        let _target_is_sethash_incdec = declared_type_incdec
            .as_deref()
            .is_some_and(|t| t == "SetHash");
        let idx_val = self.stack.pop().unwrap_or(Value::Nil);
        let key = idx_val.to_string_value();
        let container = self.get_env_with_main_alias(&name);
        // `$c[0]++` / `$c<a>++` on a Capture: when the element is a shared
        // `ContainerRef` cell (built from `\($a)` / `\(:$a)`), increment *through*
        // the cell so the original variable observes the change. The generic
        // Hash/Array path below does not understand Captures.
        if let Some(Value::Capture { positional, named }) = container.as_ref() {
            let elem = if let Ok(i) = key.parse::<usize>() {
                positional.get(i).cloned()
            } else {
                named.get(&key).cloned()
            };
            if let Some(Value::ContainerRef(arc)) = elem {
                let inner = arc.lock().unwrap().clone();
                let effective = Self::normalize_incdec_source(inner);
                let new_val = if increment {
                    self.increment_value_smart(&effective)?
                } else {
                    self.decrement_value_smart(&effective)?
                };
                arc.lock().unwrap().clone_from(&new_val);
                self.stack
                    .push(if return_new { new_val } else { effective });
                return Ok(());
            }
        }
        // `$n[0]++` / `$h<k>++` where `$n`/`$h` is itself a shared `ContainerRef`
        // cell (a scalar-bound container param, `my $s = @a`, etc.): increment the
        // element *through* the cell so the caller's container observes it. The
        // generic Array/Hash read/writeback paths below only match plain
        // containers, so a `ContainerRef` would otherwise read `Nil` and discard
        // the write (unlike `$n[0] = …` / `$n[0] += …`, which descend the cell).
        // `$n[0]++` / `$h<k>++` where the variable resolves to a raw `ContainerRef`
        // cell (a positional scalar-bound container param, `my $s := @a`, etc.):
        // increment the element *through* the cell so the caller observes it. The
        // generic Array/Hash read/writeback paths below match plain containers
        // only, so a raw cell would read `Nil` and discard the write. (Named
        // params resolve to a deref'd-but-Arc-shared plain container instead, which
        // the strong_count>1 in-place writeback below handles.)
        if let Some(Value::ContainerRef(arc)) = container.as_ref() {
            let inner = arc.lock().unwrap().clone();
            let current = match &inner {
                Value::Hash(h) => h.get(&key).cloned().unwrap_or(Value::Nil),
                Value::Array(arr, ..) => key
                    .parse::<usize>()
                    .ok()
                    .and_then(|i| arr.get(i).cloned())
                    .unwrap_or(Value::Nil),
                _ => Value::Nil,
            };
            let effective = Self::normalize_incdec_source(match current {
                Value::Nil => Self::value_carried_default(&inner)
                    .or_else(|| self.var_default(&name).cloned())
                    .filter(|d| !matches!(d, Value::Nil))
                    .unwrap_or(Value::Int(0)),
                other => other,
            });
            let new_val = if increment {
                self.increment_value_smart(&effective)?
            } else {
                self.decrement_value_smart(&effective)?
            };
            let mut updated = inner;
            match &mut updated {
                Value::Hash(h) => {
                    Value::hash_insert_through(
                        &mut Arc::make_mut(h).map,
                        key.clone(),
                        new_val.clone(),
                    );
                }
                Value::Array(arr, ..) => {
                    if let Ok(i) = key.parse::<usize>() {
                        let a = Arc::make_mut(arr);
                        Self::autoviv_resize(a, i + 1, Value::Nil)?;
                        a[i] = new_val.clone();
                    }
                }
                _ => {}
            }
            *arc.lock().unwrap() = updated;
            self.stack
                .push(if return_new { new_val } else { effective });
            return Ok(());
        }
        let current = if let Some(container_value) = container.as_ref() {
            match container_value {
                Value::Hash(h) => h.get(&key).cloned().unwrap_or(Value::Nil),
                Value::Array(arr, ..) => {
                    if let Ok(i) = key.parse::<usize>() {
                        arr.get(i).cloned().unwrap_or(Value::Nil)
                    } else {
                        Value::Nil
                    }
                }
                Value::Mix(mix, _) => mix
                    .get(&key)
                    .map_or(Value::Int(0), |w| Self::mix_weight_as_value(*w)),
                Value::Set(set, _) => {
                    if set.contains(&key) {
                        Value::Bool(true)
                    } else {
                        Value::Bool(false)
                    }
                }
                Value::Bag(bag, _) => {
                    Value::from_bigint(bag.get(&key).cloned().unwrap_or_default())
                }
                _ => Value::Nil,
            }
        } else {
            Value::Nil
        };
        let effective = match &current {
            Value::Nil => {
                // Check if the container has an `is default(...)` value;
                // e.g. `my @a is default(42); @a[0]++` should increment 42.
                // Prefer the value-carried default (HashData/ArrayData) so it
                // works when the container arrived via a parameter (whose name
                // is not in the name-keyed `var_defaults` table).
                let def = container
                    .as_ref()
                    .and_then(Self::value_carried_default)
                    .or_else(|| self.var_default(&name).cloned());
                match def {
                    Some(d) if !matches!(d, Value::Nil) => d,
                    _ => Value::Int(0),
                }
            }
            other => other.clone(),
        };
        let effective = Self::normalize_incdec_source(effective);
        let new_val = if increment {
            self.increment_value_smart(&effective)?
        } else {
            self.decrement_value_smart(&effective)?
        };
        // A Bag/BagHash holds non-negative integer counts: decrementing a weight
        // below 0 clamps the *returned* (and stored) value to 0 (the element is
        // then removed), so `--$bh<k>` on an absent key returns 0, not -1. Mix
        // weights are unbounded and keep their negative value.
        let target_is_bag = matches!(container.as_ref(), Some(Value::Bag(..)))
            || matches!(
                container.as_ref(),
                Some(Value::Package(sym)) if sym.resolve() == "BagHash"
            )
            || declared_type_incdec.as_deref() == Some("BagHash")
            || declared_constraint_incdec.as_deref() == Some("BagHash");
        let new_val = if target_is_bag {
            match &new_val {
                Value::Int(n) if *n < 0 => Value::Int(0),
                _ => new_val,
            }
        } else {
            new_val
        };
        // Type-check the incremented value against the element constraint of a
        // typed array/hash, e.g. `subset Y of Int where 1..10; my Y @x; @x[0]=10;
        // @x[0]++` must throw when the new value (11) falls outside the subset.
        // Native arrays wrap instead of erroring, so skip them. Skip container-type
        // constraints (e.g. `%h is SetHash`), where the constraint names the whole
        // container rather than its element/value type.
        if (name.starts_with('@') || name.starts_with('%'))
            && let Some(constraint) = declared_constraint_incdec.as_deref()
            && !crate::runtime::native_types::is_native_array_element_type(constraint)
            && !matches!(constraint, "num" | "num32" | "num64" | "str")
            && !matches!(
                constraint,
                "Hash"
                    | "Array"
                    | "Map"
                    | "List"
                    | "Bag"
                    | "Set"
                    | "Mix"
                    | "BagHash"
                    | "SetHash"
                    | "MixHash"
                    | "Seq"
            )
            && !self.is_container_subclass(constraint)
            && !matches!(&new_val, Value::Nil)
            && !self.type_matches_value(constraint, &new_val)
        {
            return Err(runtime::utils::type_check_element_typed_error(
                &name, constraint, &new_val,
            ));
        }
        // Modify the container in-place in the env to preserve Arc sharing
        // (e.g. when two variables reference the same array via Arc).
        // First try to modify via env_mut().get_mut() to avoid clone.
        let modified_in_place = if let Some(container_value) = self.env_mut().get_mut(&name) {
            match container_value {
                Value::Hash(h) => {
                    // Mirror the array arm below: when the hash Arc is shared
                    // (strong_count > 1) via a scalar-bound `ContainerRef` cell
                    // (`sub f($h){ $h<k>++ }` / `my $s = %h; $s<k>++`), mutate it
                    // in place so the caller observes the change. `Arc::make_mut`
                    // would COW-detach and silently drop the write (the array
                    // path already special-cased this; the hash path did not).
                    let use_inplace = Arc::strong_count(h) > 1 && !name.starts_with('%');
                    if use_inplace {
                        // SAFETY: aliased in-place mutation of a shared hash
                        // (strong_count > 1, the shared-cell case); mirrors the
                        // array arm's `arc_contents_mut` usage.
                        let h = unsafe { crate::value::arc_contents_mut(h) };
                        Value::hash_insert_through(&mut h.map, key.clone(), new_val.clone());
                    } else {
                        Value::hash_insert_through(
                            &mut Arc::make_mut(h).map,
                            key.clone(),
                            new_val.clone(),
                        );
                    }
                    true
                }
                Value::Array(arr, ..) => {
                    if let Ok(i) = idx_val.to_string_value().parse::<usize>() {
                        // Use in-place mutation when the array is shared
                        // (strong_count > 1) to preserve identity semantics,
                        // matching the behavior of index assignment.
                        let use_inplace = Arc::strong_count(arr) > 1 && !name.starts_with('@');
                        let a: &mut crate::value::ArrayData = if use_inplace {
                            // SAFETY: aliased in-place mutation of a shared array
                            // (strong_count > 1, the case that needs the shared
                            // write); see `arc_contents_mut`.
                            unsafe { crate::value::arc_contents_mut(arr) }
                        } else {
                            Arc::make_mut(arr)
                        };
                        // Fill holes with the element type's type object for a
                        // typed array (e.g. `my Int @a; @a[4]++` leaves `(Int)`
                        // placeholders), or 0/0.0/"" for native arrays.
                        let fill =
                            Self::native_fill_for_constraint(declared_constraint_incdec.as_deref());
                        Self::autoviv_resize(a, i + 1, fill)?;
                        a[i] = new_val.clone();
                        true
                    } else {
                        false
                    }
                }
                Value::Mix(mix, is_mutable) => {
                    if !*is_mutable {
                        return Err(RuntimeError::assignment_ro(Some("Mix")));
                    }
                    let weight = Self::mix_assignment_weight(&new_val)?;
                    let m = Arc::make_mut(mix);
                    if new_val.truthy() {
                        m.insert(key.clone(), weight);
                    } else {
                        m.remove(&key);
                    }
                    true
                }
                Value::Set(set, is_mutable) => {
                    if !*is_mutable {
                        return Err(RuntimeError::assignment_ro(Some("Set")));
                    }
                    let s = Arc::make_mut(set);
                    if new_val.truthy() {
                        s.insert(key.clone());
                    } else {
                        s.remove(&key);
                    }
                    true
                }
                Value::Bag(bag, is_mutable) => {
                    if !*is_mutable {
                        return Err(RuntimeError::assignment_ro(Some("Bag")));
                    }
                    let b = Arc::make_mut(bag);
                    let n = match &new_val {
                        Value::Int(i) => num_bigint::BigInt::from(*i),
                        Value::BigInt(big) => (**big).clone(),
                        _ => num_bigint::BigInt::from(0),
                    };
                    if num_traits::Signed::is_positive(&n) {
                        b.insert(key.clone(), n);
                    } else {
                        b.remove(&key);
                    }
                    true
                }
                // Autovivify typed variables: `my MixHash $mh; $mh<key>++`
                Value::Package(sym) => {
                    let type_name = sym.resolve();
                    match type_name.as_str() {
                        "MixHash" => {
                            let mut weights = HashMap::new();
                            let weight = Self::mix_assignment_weight(&new_val)?;
                            if new_val.truthy() {
                                weights.insert(key.clone(), weight);
                            }
                            *container_value = Value::mix_hash(weights);
                            true
                        }
                        "BagHash" => {
                            let mut counts = HashMap::new();
                            if let Value::Int(n) = &new_val
                                && *n > 0
                            {
                                counts.insert(key.clone(), *n);
                            }
                            *container_value = Value::bag_hash(counts);
                            true
                        }
                        "SetHash" => {
                            let mut items = std::collections::HashSet::new();
                            if new_val.truthy() {
                                items.insert(key.clone());
                            }
                            *container_value =
                                Value::Set(Arc::new(crate::value::SetData::new(items)), true);
                            true
                        }
                        _ => false,
                    }
                }
                _ => false,
            }
        } else {
            false
        };
        if modified_in_place {
            // Update local slot to match the modified env value
            if let Some(val) = self.env().get(&name).cloned() {
                self.update_local_if_exists(code, &name, &val);
            }
            // Inside a critical section, propagate the whole mutated aggregate to
            // the shared store: the COW element write above landed only in this
            // thread's local env (an `@`/`%` name COW-detaches, dropping the
            // shared Arc link), so the next lock holder must re-read it on entry.
            self.writeback_critical_var(&name);
        } else {
            // Autovivify typed containers for inc/dec on undefined variables
            let constraint = loan_env!(self, var_type_constraint(&name));
            let effective_type = declared_type_incdec.as_deref().or(constraint.as_deref());
            if let Some(type_name) = effective_type
                && matches!(type_name, "MixHash" | "BagHash" | "SetHash")
            {
                let new_container = match type_name {
                    "MixHash" => {
                        let mut weights = HashMap::new();
                        let weight = Self::mix_assignment_weight(&new_val)?;
                        if new_val.truthy() {
                            weights.insert(key.clone(), weight);
                        }
                        Value::mix_hash(weights)
                    }
                    "BagHash" => {
                        let mut counts = HashMap::new();
                        if let Value::Int(n) = &new_val
                            && *n > 0
                        {
                            counts.insert(key.clone(), *n);
                        }
                        Value::bag_hash(counts)
                    }
                    "SetHash" => {
                        let mut items = std::collections::HashSet::new();
                        if new_val.truthy() {
                            items.insert(key.clone());
                        }
                        Value::Set(Arc::new(crate::value::SetData::new(items)), true)
                    }
                    _ => unreachable!(),
                };
                self.env_mut().insert(name.clone(), new_container);
                if let Some(val) = self.env().get(&name).cloned() {
                    self.update_local_if_exists(code, &name, &val);
                }
            }
        }
        if return_new {
            self.stack.push(new_val);
        } else {
            self.stack.push(effective);
        }
        Ok(())
    }
}
