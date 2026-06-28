//! Fast-path `@arr.push(val)` op, split from `vm_data_ops` (§7-8 file split).
use super::*;
use crate::value::RuntimeError;

impl Interpreter {
    /// Fast path for @arr.push(val) — directly appends to the array Arc.
    pub(super) fn exec_array_push_op(
        &mut self,
        code: &CompiledCode,
        target_name_idx: u32,
        value_source_idx: Option<u32>,
    ) -> Result<(), RuntimeError> {
        let target_name = Self::const_str(code, target_name_idx);
        // A lazy `@`-array (infinite source) cannot be pushed to: there is no
        // end to append after. raku throws `X::Cannot::Lazy`
        // ("Cannot push to a lazy list onto a Array"). (L2)
        if let Some(Value::LazyList(ll)) = self.env().get(target_name)
            && ll.in_array_context()
            && ll.is_genuinely_lazy()
        {
            let _ = self.stack.pop();
            return Err(RuntimeError::cannot_lazy_with_action("push to", "Array"));
        }
        // Shared (threaded) context: route an Array push through the atomic
        // shared store so concurrent `@a.push` from multiple threads serialize
        // under the shared_vars write lock instead of clobbering each other's
        // stale local snapshots (lost update). Non-Array targets keep the
        // interpreter fallback.
        if self.shared_vars_active {
            let val = self.stack.pop().unwrap_or(Value::Nil);
            let target = self.env().get(target_name).cloned().unwrap_or(Value::Nil);
            // Only a plain lexical `@name` (not an attribute `@!x`/`@.x` or other
            // twigil'd form) has a single shared identity across threads, so only
            // it may funnel into the name-keyed atomic shared store.
            if matches!(target, Value::Array(..) | Value::Nil)
                && Self::is_plain_lexical_array_name(target_name)
            {
                let items = match val {
                    Value::Slip(items) => items.to_vec(),
                    other => vec![other],
                };
                let result = self.shared_array_extend(target_name, items, false);
                self.stack.push(result);
                return Ok(());
            }
            let result = loan_env!(self, call_method_with_values(target, "push", vec![val]))?;
            self.stack.push(result);
            return Ok(());
        }
        // TODO: compile to bytecode — shaped-array push, blocked-by: shaped
        // dimension metadata check in Interpreter. See ledger §1.
        // Check for shaped arrays — must fall back to interpreter
        // (push is illegal on fixed-dimension arrays)
        if let Some(Value::Array(_, kind)) = self.env().get(target_name)
            && *kind == crate::value::ArrayKind::Shaped
        {
            let val = self.stack.pop().unwrap_or(Value::Nil);
            let target = self.env().get(target_name).cloned().unwrap_or(Value::Nil);
            let result = loan_env!(self, call_method_with_values(target, "push", vec![val]))?;
            self.stack.push(result);
            return Ok(());
        }
        let mut val = self.stack.pop().unwrap_or(Value::Nil);

        // Reference push (`@a.push(@b)` / `@a.push(%h)`): Raku's non-flattening
        // `**@` slurpy stores the container itself, so later mutations of the
        // source (`@b.push(4)`, `@b[0]=v`, `@b=(...)`) must propagate to the
        // stored element. Share a `ContainerRef` cell between the source variable
        // and the pushed element (the same mechanism as a whole-container `:=`
        // bind). Reuse the source's existing cell if it already has one.
        if let Some(src_idx) = value_source_idx
            && matches!(val, Value::Array(..) | Value::Hash(..))
        {
            let src_name = Self::const_str(code, src_idx).to_string();
            let existing_cell = self
                .get_env_with_main_alias(&src_name)
                .or_else(|| {
                    self.find_local_slot(code, &src_name)
                        .map(|s| self.locals[s].clone())
                })
                .and_then(|v| match v {
                    Value::ContainerRef(cell) => Some(cell),
                    _ => None,
                });
            let cell = existing_cell.unwrap_or_else(|| {
                let cell = std::sync::Arc::new(std::sync::Mutex::new(val.clone()));
                let cell_val = Value::ContainerRef(cell.clone());
                self.set_env_with_main_alias(&src_name, cell_val.clone());
                self.update_local_if_exists(code, &src_name, &cell_val);
                cell
            });
            val = Value::ContainerRef(cell);
        }

        // Empty (empty Slip) means nothing to push -- return the array as-is.
        if let Value::Slip(ref items) = val
            && items.is_empty()
        {
            let result = self.env().get(target_name).cloned().unwrap_or(Value::Nil);
            self.stack.push(result);
            return Ok(());
        }

        // TODO: compile to bytecode — non-simple-target push, blocked-by:
        // first-class container identity Phase 2 (closure-captured ContainerRef
        // arrays). See ledger §1.
        // Check the target exists as a simple Array in env.
        // If not (e.g., captured closure var, or non-Array), fall back to interpreter.
        let is_simple_array = self
            .env()
            .get(target_name)
            .is_some_and(|v| matches!(v, Value::Array(..)));
        if !is_simple_array {
            let target = self.env().get(target_name).cloned().unwrap_or(Value::Nil);
            // Phase 2 Stage 2: a `:=`-cell-bound variable (`@x[0] := @b` /
            // `%h<k> := @b`) or a Slice 2a `=`-array-shared scalar (`$n = @z`)
            // holds a shared `ContainerRef` cell. Mutate the array INSIDE the
            // cell with COW (`Arc::make_mut`) — mirroring the simple-array path
            // below — so a copy made out of this cell (`my @copy = @z`), which
            // shares the inner Arc, is detached rather than mutated in place.
            // The shared cell itself keeps every alias coherent.
            if let Value::ContainerRef(cell) = target {
                let mut guard = cell.lock().unwrap();
                if let Value::Array(arr, kind) = &mut *guard {
                    let kind = *kind;
                    let items = Arc::make_mut(arr);
                    match &val {
                        Value::Slip(slip_items) => items.extend(slip_items.iter().cloned()),
                        _ => items.push(val),
                    }
                    let result = Value::Array(arr.clone(), kind);
                    drop(guard);
                    self.stack.push(result);
                    return Ok(());
                }
                // Non-array inner (e.g. Hash): generic clone-and-write-back.
                let inner = guard.clone();
                drop(guard);
                let result = loan_env!(self, call_method_with_values(inner, "push", vec![val]))?;
                *cell.lock().unwrap() = result.clone();
                self.stack.push(result);
                return Ok(());
            }
            let result = loan_env!(self, call_method_with_values(target, "push", vec![val]))?;
            self.stack.push(result);
            return Ok(());
        }

        // Check type constraint on the array variable.
        // For Slip values, check each element individually.
        if let Some(type_name) = self
            .var_type_constraint_fast(target_name)
            .map(|s| s.to_string())
        {
            let items_to_check: Vec<&Value> = match &val {
                Value::Slip(items) => items.iter().collect(),
                other => vec![other],
            };
            for item in items_to_check {
                if !self.type_matches_value(&type_name, item) {
                    return Err(RuntimeError::typecheck_assignment(
                        &type_name,
                        item,
                        Some(target_name),
                    ));
                }
            }
        }

        // Find the local slot and drop it to allow in-place mutation
        let local_slot = self.find_local_slot(code, target_name);
        if let Some(slot) = local_slot {
            self.locals[slot] = Value::Nil;
        }

        let result = if let Some(Value::Array(arr, kind)) = self.env_mut().get_mut(target_name) {
            let items = Arc::make_mut(arr);
            match &val {
                Value::Slip(slip_items) => items.extend(slip_items.iter().cloned()),
                _ => items.push(val),
            }
            Value::Array(arr.clone(), *kind)
        } else {
            // Auto-vivify: create new array
            let arr = match &val {
                Value::Slip(slip_items) => Value::real_array(slip_items.to_vec()),
                _ => Value::real_array(vec![val]),
            };
            self.env_mut().insert(target_name.to_string(), arr.clone());
            arr
        };

        // Restore local slot
        if let Some(slot) = local_slot {
            self.locals[slot] = result.clone();
        }

        self.stack.push(result);
        // Slice 6.3 step 2: no env_dirty mark. This native push path mutates only
        // `target_name` in env and has already reverse-write-through'd the result
        // into its local slot just above, so the caller's slot is coherent — a
        // pull would be redundant. (The interpreter-fallback push branches above,
        // for shared/shaped/non-simple-array targets, keep their conservative mark.)
        Ok(())
    }
}
