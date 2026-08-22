//! Fast-path `@arr.push(val)` op, split from `vm_data_ops` (§7-8 file split).
use super::*;
use crate::value::RuntimeError;

impl Interpreter {
    /// Storing Nil into a fresh array element resets it to the element
    /// default: `is default(...)` first, then the native element zero, then
    /// the element type object for typed (`my Int @a; @a.push(Nil)` stores
    /// `Int`), else `Any` for untyped. Slips convert per element.
    ///
    /// ADR-0049 slice 4: routed through `assign_store_nil_default` (the same
    /// helper the element-assign ladder and the whole-container list-assign
    /// fixups already use), so `push`/`append`/`unshift`/`prepend`/`splice`
    /// agree with plain element assignment on which default a `Nil` decays
    /// to — this used to only check the declared TYPE via
    /// `element_constraint_for` (ADR-0042 slice 1's routing) and therefore
    /// silently ignored a container's own `is default(...)` value, storing
    /// the bare `Any` element instead: `my @a is default(42) = 1,2,3;
    /// @a.push(Nil); @a.raku` stored `[1, 2, 3, Any]` where raku stores
    /// `[1, 2, 3, 42]` (the read-side `@a[3]` already answered `42` before
    /// this fix, via a DIFFERENT, unrelated read-chokepoint compensation for
    /// in-range `Any` elements — see ADR-0049 §8's Row 29 follow-up note —
    /// but `.raku`/`eqv`/`.List` all read the raw stored element, so the
    /// mismatch was real and visible there).
    fn push_nil_to_elem_default(&mut self, target_name: &str, val: Value) -> Value {
        let has_nil = match val.view() {
            ValueView::Slip(items) => items.iter().any(Value::is_nil),
            _ => val.is_nil(),
        };
        if !has_nil {
            return val;
        }
        let target = self
            .env()
            .get(target_name)
            .cloned()
            .unwrap_or_else(|| Value::real_array(Vec::new()));
        let default = self.assign_store_nil_default(target_name, &target);
        match val.view() {
            ValueView::Slip(items) => Value::slip(
                items
                    .iter()
                    .map(|v| {
                        if v.is_nil() {
                            default.clone()
                        } else {
                            v.clone()
                        }
                    })
                    .collect(),
            ),
            _ => default,
        }
    }

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
            let val = self.push_nil_to_elem_default(target_name, val);
            // The declared element type governs this push exactly as it does the
            // single-threaded one below. `shared_vars_active` latches on for the
            // rest of the process at the first `start`, so without this a
            // `my uint32 @W` stopped truncating to 32 bits once ANY thread had
            // been spawned anywhere — and `Digest::SHA1`, whose message schedule
            // relies on that truncation, then produced a silently wrong digest.
            self.check_push_element_type(target_name, &val)?;
            let val = self.wrap_native_int_push_value(target_name, val);
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
                    // ADR-0040 slice 1: itemize per element, after Slip
                    // expansion, so arity is unaffected and only the stored
                    // value gains the Scalar-container property.
                    let items: Vec<Value> = match val.view() {
                        ValueView::Slip(items) => {
                            items.iter().cloned().map(Self::itemize_value).collect()
                        }
                        _ => vec![Self::itemize_value(val)],
                    };
                    let mut guard = cell.lock().unwrap_or_else(|e| e.into_inner());
                    (*guard).with_array_mut(|arc, _| {
                        let data = crate::gc::Gc::make_mut(arc);
                        data.items_mut().extend(items);
                    });
                    let result = guard.clone();
                    drop(guard);
                    self.stack.push(result);
                    return Ok(());
                }
            }
            // Only a plain lexical `@name` (not an attribute `@!x`/`@.x` or other
            // twigil'd form) has a single shared identity across threads, so only
            // it may funnel into the name-keyed atomic shared store — and only
            // when this frame can actually be racing: a worker thread (where the
            // whole point is to serialize concurrent appends), or a name that is
            // GENUINELY shared already. The store is keyed by NAME, so routing a
            // *main-thread* frame-local `my @a` through it detaches it from every
            // other binding of the same container — a `my @t := @a` alias keeps
            // the original node while the push lands under
            // `__mutsu_atomic_arr::@a`. `shared_vars_active` never goes back to
            // false, so without this gate every array push in a program that once
            // spawned a thread is name-keyed. Mirrors the "genuinely shared" gate
            // `assign_array_elem_to_shared_var` already applies. A name this
            // lineage re-declared is frame-local on a worker thread too — the
            // store is keyed by name for the WHOLE process, so a worker's own
            // `my @a` would otherwise be the parent's `@a`.
            if matches!(target.view(), ValueView::Array(..) | ValueView::Nil)
                && Self::is_plain_lexical_array_name(target_name)
                && !self.container_name_is_redeclared(target_name)
                && (self.is_thread_clone() || self.array_name_is_shared(target_name))
            {
                // ADR-0040 slice 1: itemize per element, after Slip expansion.
                let items: Vec<Value> = match val.view() {
                    ValueView::Slip(items) => {
                        items.iter().cloned().map(Self::itemize_value).collect()
                    }
                    _ => vec![Self::itemize_value(val)],
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
        let mut val = {
            let popped = self.stack.pop().unwrap_or(Value::NIL);
            self.push_nil_to_elem_default(target_name, popped)
        };

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
            // ADR-0040 slice 1: the pushed ELEMENT (read back via `@a[i]`)
            // is itemized -- `@a.push(@b); @a[0].raku` is `$[1, 2]` in
            // raku, not `[1, 2]` -- even though `@b` read directly stays
            // bare (`@b.raku` is `[1, 2]`). Wrapping the shared `ContainerRef`
            // itself in an outer `Scalar` (rather than flipping the cell's
            // own inner `ArrayKind`) keeps the two readers independent: the
            // cell's content is untouched, so `@b`'s own binding (which
            // reads the bare `ContainerRef` directly) is unaffected, while
            // `@a[i]`'s element holds the Scalar-wrapped alias.
            val = Value::container_ref(cell).item();
        }

        // Empty (empty Slip) means nothing to push -- return the array as-is.
        if let ValueView::Slip(items) = val.view()
            && items.is_empty()
        {
            let result = self.env().get(target_name).cloned().unwrap_or(Value::NIL);
            self.stack.push(result);
            return Ok(());
        }

        // The declared element type governs the push whatever SHAPE the target
        // has. It is checked and applied here, above the target dispatch, because
        // a plain lexical does not stay a plain `Array` for a program's whole
        // life: once any `start` runs, `shared_vars_active` wraps lexicals in a
        // `ContainerRef` cell, which took the `!is_simple_array` branch below and
        // skipped both. `my uint32 @W` therefore stopped truncating to 32 bits
        // after the first thread spawned anywhere in the process — and since
        // `Digest::SHA1`'s message schedule relies on that truncation, a `sha1`
        // call after any `start` silently produced a WRONG digest.
        self.check_push_element_type(target_name, &val)?;
        let val = self.wrap_native_int_push_value(target_name, val);

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
                // ADR-0040 slice 1: itemize per element, after Slip expansion.
                let pushed = inner
                    .with_array_inplace(|data, _| {
                        let val = val_slot.take().expect("push value present");
                        match val.view() {
                            ValueView::Slip(slip_items) => data
                                .items_mut()
                                .extend(slip_items.iter().cloned().map(Self::itemize_value)),
                            _ => data.items_mut().push(Self::itemize_value(val)),
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

        let mut val_slot = Some(val);
        // Container identity (§3): append through the shared backing node —
        // no COW, no local-slot zeroing dance — so every by-value holder of
        // the same array (a `(0, @a)` capture, an element) sees the push.
        // ADR-0040 slice 1: itemize per element, after Slip expansion.
        let target = self.env().get(target_name).cloned();
        let pushed = target.as_ref().and_then(|v| {
            v.with_array_inplace(|data, _| {
                let val = val_slot.take().expect("push value present");
                match val.view() {
                    ValueView::Slip(slip_items) => data
                        .items_mut()
                        .extend(slip_items.iter().cloned().map(Self::itemize_value)),
                    _ => data.items_mut().push(Self::itemize_value(val)),
                }
            })
        });
        let result = match pushed {
            Some(()) => target.expect("array target present"),
            None => {
                let val = val_slot.take().expect("push value present");
                // Auto-vivify: create new array
                let arr = match val.view() {
                    ValueView::Slip(slip_items) => Value::real_array(
                        slip_items
                            .iter()
                            .cloned()
                            .map(Self::itemize_value)
                            .collect(),
                    ),
                    _ => Value::real_array(vec![Self::itemize_value(val)]),
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

    /// Type-check a pushed value (or every element of a pushed `Slip`) against
    /// the declared element type of `target_name`. A no-op for an untyped array.
    ///
    /// ADR-0042 slice 1: constraint comes from `element_constraint_for`
    /// (container-embedded metadata first, name-keyed map as fallback) rather
    /// than the map-only `var_type_constraint_fast` — this is the hot
    /// `@a.push` chokepoint the ADR names for the bench-CI watch.
    fn check_push_element_type(
        &mut self,
        target_name: &str,
        val: &Value,
    ) -> Result<(), RuntimeError> {
        let target = self.env().get(target_name).cloned().unwrap_or(Value::NIL);
        let Some(type_name) = self.element_constraint_for(target_name, &target) else {
            return Ok(());
        };
        // Owned clone of the Slip backing (a view guard's borrow cannot
        // outlive the match), so the item refs stay valid for the loop.
        let slip_items: Option<std::sync::Arc<Vec<Value>>> = match val.view() {
            ValueView::Slip(items) => Some(items.clone()),
            _ => None,
        };
        let items_to_check: Vec<&Value> = match &slip_items {
            Some(items) => items.iter().collect(),
            None => vec![val],
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
        Ok(())
    }

    /// Wrap a pushed value (or every element of a pushed `Slip`) to the native
    /// integer width of `target_name`'s element type. A no-op for boxed arrays.
    fn wrap_native_int_push_value(&mut self, target_name: &str, val: Value) -> Value {
        let Some(constraint) = self.native_int_element_constraint(target_name) else {
            return val;
        };
        match val.view() {
            ValueView::Slip(items) => Value::slip(
                items
                    .iter()
                    .map(|v| Self::wrap_native_int_by_constraint(&constraint, v.clone()))
                    .map(|r| r.unwrap_or(Value::NIL))
                    .collect(),
            ),
            _ => Self::wrap_native_int_by_constraint(&constraint, val.clone()).unwrap_or(val),
        }
    }

    /// The native integer element type of the array variable `name`, if any.
    ///
    /// ADR-0042 slice 1: routed through `element_constraint_for` (see
    /// `check_push_element_type`) instead of the map-only
    /// `var_type_constraint_fast`.
    pub(crate) fn native_int_element_constraint(&mut self, name: &str) -> Option<String> {
        let target = self.env().get(name).cloned().unwrap_or(Value::NIL);
        let constraint = self.element_constraint_for(name, &target)?;
        crate::runtime::native_types::is_native_int_type(&constraint).then_some(constraint)
    }

    /// Wrap every element of a multi-argument push/append onto a native integer
    /// array (`my uint8 @e; @e.push(1, 300, 2)` stores `1, 44, 2`).
    pub(crate) fn wrap_native_int_items(&mut self, name: &str, items: Vec<Value>) -> Vec<Value> {
        let Some(constraint) = self.native_int_element_constraint(name) else {
            return items;
        };
        items
            .into_iter()
            .map(|v| Self::wrap_native_int_by_constraint(&constraint, v.clone()).unwrap_or(v))
            .collect()
    }
}
