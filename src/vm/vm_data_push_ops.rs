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
        if let Some(ValueView::LazyList(ll)) = self.env().get(target_name).map(Value::view)
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
            let val = self.stack.pop().unwrap_or(Value::NIL);
            let target = self.env().get(target_name).cloned().unwrap_or(Value::NIL);
            // Track B/Track C: a `state @a` under an active thread context is a
            // shared `ContainerRef` cell. Push INTO the cell under its lock
            // (COW of the inner node keeps escaped snapshots immutable), so
            // every holder — other calls, other threads, the state store —
            // sees the append. Previously this fell through to the plain
            // method dispatch with the raw cell as invocant and failed with
            // "No such method 'push'" once the cell was non-empty.
            if let ValueView::ContainerRef(cell) = target.view() {
                let is_cell_array = matches!(
                    cell.lock().unwrap_or_else(|e| e.into_inner()).view(),
                    ValueView::Array(..)
                );
                if is_cell_array {
                    let items = match val.view() {
                        ValueView::Slip(items) => items.to_vec(),
                        _ => vec![val],
                    };
                    let mut guard = cell.lock().unwrap_or_else(|e| e.into_inner());
                    (*guard).with_array_mut(|arc, _| {
                        let data = crate::gc::Gc::make_mut(arc);
                        data.items.extend(items);
                    });
                    let result = guard.clone();
                    drop(guard);
                    self.stack.push(result);
                    return Ok(());
                }
            }
            // Only a plain lexical `@name` (not an attribute `@!x`/`@.x` or other
            // twigil'd form) has a single shared identity across threads, so only
            // it may funnel into the name-keyed atomic shared store.
            if matches!(target.view(), ValueView::Array(..) | ValueView::Nil)
                && Self::is_plain_lexical_array_name(target_name)
            {
                let items = match val.view() {
                    ValueView::Slip(items) => items.to_vec(),
                    _ => vec![val],
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
        if let Some(ValueView::Array(_, kind)) = self.env().get(target_name).map(Value::view)
            && kind == crate::value::ArrayKind::Shaped
        {
            let val = self.stack.pop().unwrap_or(Value::NIL);
            let target = self.env().get(target_name).cloned().unwrap_or(Value::NIL);
            let result = loan_env!(self, call_method_with_values(target, "push", vec![val]))?;
            self.stack.push(result);
            return Ok(());
        }
        let mut val = self.stack.pop().unwrap_or(Value::NIL);

        // Reference push (`@a.push(@b)` / `@a.push(%h)`): Raku's non-flattening
        // `**@` slurpy stores the container itself, so later mutations of the
        // source (`@b.push(4)`, `@b[0]=v`, `@b=(...)`) must propagate to the
        // stored element. Share a `ContainerRef` cell between the source variable
        // and the pushed element (the same mechanism as a whole-container `:=`
        // bind). Reuse the source's existing cell if it already has one.
        if let Some(src_idx) = value_source_idx
            && matches!(val.view(), ValueView::Array(..) | ValueView::Hash(..))
        {
            let src_name = Self::const_str(code, src_idx).to_string();
            let existing_cell = self
                .get_env_with_main_alias(&src_name)
                .or_else(|| {
                    self.find_local_slot(code, &src_name)
                        .map(|s| self.locals[s].clone())
                })
                .and_then(|v| match v.view() {
                    ValueView::ContainerRef(cell) => Some(cell.clone()),
                    _ => None,
                });
            let cell = existing_cell.unwrap_or_else(|| {
                let cell = crate::gc::Gc::new(std::sync::Mutex::new(val.clone()));
                let cell_val = Value::container_ref(cell.clone());
                self.set_env_with_main_alias(&src_name, cell_val.clone());
                self.update_local_if_exists(code, &src_name, &cell_val);
                cell
            });
            val = Value::container_ref(cell);
        }

        // Empty (empty Slip) means nothing to push -- return the array as-is.
        if let ValueView::Slip(items) = val.view()
            && items.is_empty()
        {
            let result = self.env().get(target_name).cloned().unwrap_or(Value::NIL);
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
            .is_some_and(|v| matches!(v.view(), ValueView::Array(..)));
        if !is_simple_array {
            let target = self.env().get(target_name).cloned().unwrap_or(Value::NIL);
            // Phase 2 Stage 2: a `:=`-cell-bound variable (`@x[0] := @b` /
            // `%h<k> := @b`) or a Slice 2a `=`-array-shared scalar (`$n = @z`)
            // holds a shared `ContainerRef` cell. Mutate the array INSIDE the
            // cell with COW (`Arc::make_mut`) — mirroring the simple-array path
            // below — so a copy made out of this cell (`my @copy = @z`), which
            // shares the inner Arc, is detached rather than mutated in place.
            // The shared cell itself keeps every alias coherent.
            if let ValueView::ContainerRef(cell) = target.view() {
                let cell = cell.clone();
                let guard = cell.lock().unwrap();
                let inner = guard.clone();
                drop(guard);
                // Container identity (§3): write through the shared backing
                // node so by-value holders of the same array observe the push.
                let mut val_slot = Some(val);
                let pushed = inner
                    .with_array_inplace(|data, _| {
                        let val = val_slot.take().expect("push value present");
                        match val.view() {
                            ValueView::Slip(slip_items) => {
                                data.items.extend(slip_items.iter().cloned())
                            }
                            _ => data.items.push(val),
                        }
                    })
                    .is_some();
                if pushed {
                    self.stack.push(inner);
                    return Ok(());
                }
                // Non-array inner (e.g. Hash): generic clone-and-write-back.
                let val = val_slot.take().expect("push value present");
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
            // Owned clone of the Slip backing (a view guard's borrow cannot
            // outlive the match), so the item refs stay valid for the loop.
            let slip_items: Option<std::sync::Arc<Vec<Value>>> = match val.view() {
                ValueView::Slip(items) => Some(items.clone()),
                _ => None,
            };
            let items_to_check: Vec<&Value> = match &slip_items {
                Some(items) => items.iter().collect(),
                None => vec![&val],
            };
            for item in items_to_check {
                if !self.type_matches_value(&type_name, item) {
                    // A rejected element push reports "for an element of @a"
                    // (matching rakudo and the interpreter's other array-mutator
                    // paths), not the scalar "in assignment to @a" wording.
                    return Err(crate::runtime::utils::type_check_element_typed_error(
                        target_name,
                        &type_name,
                        item,
                    ));
                }
            }
        }

        let mut val_slot = Some(val);
        // Container identity (§3): append through the shared backing node —
        // no COW, no local-slot zeroing dance — so every by-value holder of
        // the same array (a `(0, @a)` capture, an element) sees the push.
        let target = self.env().get(target_name).cloned();
        let pushed = target.as_ref().and_then(|v| {
            v.with_array_inplace(|data, _| {
                let val = val_slot.take().expect("push value present");
                match val.view() {
                    ValueView::Slip(slip_items) => data.items.extend(slip_items.iter().cloned()),
                    _ => data.items.push(val),
                }
            })
        });
        let result = match pushed {
            Some(()) => target.expect("array target present"),
            None => {
                let val = val_slot.take().expect("push value present");
                // Auto-vivify: create new array
                let arr = match val.view() {
                    ValueView::Slip(slip_items) => Value::real_array(slip_items.to_vec()),
                    _ => Value::real_array(vec![val]),
                };
                self.env_mut().insert(target_name.to_string(), arr.clone());
                arr
            }
        };

        // Keep the local slot coherent with env (dual-store write-through).
        self.update_local_if_exists(code, target_name, &result);

        self.stack.push(result);
        // Slice 6.3 step 2: no env_dirty mark. This native push path mutates only
        // `target_name` in env and has already reverse-write-through'd the result
        // into its local slot just above, so the caller's slot is coherent — a
        // pull would be redundant. (The interpreter-fallback push branches above,
        // for shared/shaped/non-simple-array targets, keep their conservative mark.)
        Ok(())
    }
}
