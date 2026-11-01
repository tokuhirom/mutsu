//! ADR-0067's subscript-receiver producer: `@a[0].mut` and `%h<a>.mut` hand the
//! element's own container to a callee that binds its invocant raw.
//!
//! Slice 3b made the *arrival* direction work by boxing the receiver's storage
//! location and arming a one-slot channel the parameter binder consumes. It
//! could only do that for a receiver the call site can **name**: `CallMethodMut`
//! carries a `target_name_idx`, and `capture_lvalue_invocant_cell` turns that
//! name into a location. A subscript receiver has neither half —
//! `--dump-bytecode` on `@a[0].mut` is
//!
//! ```text
//! GetArrayVar(0); LoadConst(3); Index { is_positional: true }; CallMethod { .. }
//! ```
//!
//! a plain `CallMethod` with no name, whose receiver is the *value* the `Index`
//! op already read out of the array. There is nothing left to box.
//!
//! So this is a **producer**, not another consumer: [`OpCode::IndexInvocantRef`]
//! hands over the element's own `Scalar` cell rather than its value, and the
//! rest of the machinery is reused verbatim —
//!
//! - `CallMethod`'s existing decontainerize chokepoint (`exec_call_method_op_impl`,
//!   "a method invocant that is a first-class element container ... is
//!   transparent to method dispatch") already makes a `ContainerRef` receiver
//!   invisible to every callee, so no new guard is needed to keep the ~40
//!   `Instance`/`Array`/`Hash` branches from seeing one;
//! - slice 3b's [`Interpreter::arm_raw_invocant_arrival`] already prefers an
//!   existing `ContainerRef` receiver over minting a cell, so it consumes what
//!   this produces with no new transport.
//!
//! **Why the emission can be unconditional and the gate cannot be the
//! compiler's.** Rawness depends on the element's runtime type (and, for the
//! dynamic spelling, on a runtime method-name string), so every
//! `<subscript>.method(...)` compiles to this op. What keeps that affordable is
//! the same set-only process-global mirror slice 3b gates on: with no
//! raw-invocant method declared anywhere in the program, the op is one relaxed
//! atomic load followed by the ordinary `Index` implementation, byte for byte.

use super::*;

impl Interpreter {
    /// `Index` in receiver position. Produces the element's container when the
    /// program could have a raw-invocant callee, and is `Index` otherwise.
    pub(crate) fn exec_index_invocant_ref_op(
        &mut self,
        is_positional: bool,
        vivify: bool,
    ) -> Result<(), RuntimeError> {
        // A Junction key threads the whole load over its values: every addressed
        // element is vivified to an `Array` and the call autothreads over a
        // Junction of the stored shared nodes (`%h{ any 0, 1 }.push: 42`).
        let junction_key = if vivify && self.stack.len() >= 2 {
            self.stack.last().cloned()
        } else {
            None
        };
        if let Some(key) = &junction_key
            && let ValueView::Junction { kind, values } = key.view()
        {
            self.stack.pop();
            let container = self.stack.pop().unwrap_or(Value::NIL);
            let mut elems = Vec::with_capacity(values.len());
            for v in values.iter() {
                self.stack.push(container.clone());
                self.stack.push(v.clone());
                self.exec_index_invocant_ref_op(is_positional, true)?;
                let element = self.stack.pop().unwrap_or(Value::NIL);
                elems.push(self.vivify_element_array(element)?);
            }
            self.stack.push(Value::junction(kind, elems));
            return Ok(());
        }
        // A vivifying method name takes the element's location even when it is
        // missing (the deferred token), so `CallMethod` can store a fresh
        // container into it; any other receiver only needs an existing cell,
        // and only when a raw-invocant callee could bind it.
        // A receiver chain compiled in container mode hands this op a location
        // rather than a container: a deferred token, an empty scalar cell, or an
        // undefined value. The deferred-walk subscript extends the path to the
        // element (creating nothing yet), as a `:=` bind to it would.
        if vivify
            && self.stack.len() >= 2
            && let Some(target) = self.stack.get(self.stack.len() - 2)
            && Self::vivify_target_is_location(target)
        {
            self.exec_index_autovivify_lazy_op(false, is_positional)?;
            if let Some(location) = self.stack.last().cloned() {
                self.vivify_typed_hole(&location)?;
            }
            return Ok(());
        }
        if (vivify || crate::runtime::raw_invocant::any_raw_invocant_method_possible())
            && let Some(cell) = self.take_subscript_element_cell(is_positional, vivify)
        {
            if vivify {
                self.vivify_typed_hole(&cell)?;
            }
            // `skip_postcircumfix_overload` is scoped to ONE subscript dispatch
            // and is consumed by `exec_index_op_with_positional`; consume it
            // here too so producing a cell instead cannot leak the suppression
            // onto the next, unrelated subscript. (An overload can only be
            // declared for an `Instance` receiver, which the producer declines,
            // so this is hygiene rather than a reachable bug today.)
            self.dispatch.skip_postcircumfix_overload = false;
            self.stack.push(cell);
            return Ok(());
        }
        self.exec_index_op_with_positional(is_positional)?;
        self.hllize_vm_collection_on_top();
        Ok(())
    }

    /// Whether the receiver a vivifying subscript is applied to is a location
    /// the deferred walk must extend (a token, a scalar cell) or an undefined
    /// value, rather than a real `Array`/`Hash` the element cell can be read from.
    // Cost: O(1).
    fn vivify_target_is_location(target: &Value) -> bool {
        match target.view() {
            ValueView::HashEntryRef { .. } | ValueView::ContainerRef(_) => true,
            ValueView::Nil => true,
            ValueView::Package(p) => p == "Any" || p == "Mu",
            _ => false,
        }
    }

    /// An element of a typed container (`my Array of Int @x`, whose cells
    /// enforce `Array[Int]`) must be a container of that type: a hole is
    /// vivified to one (a plain `Array` store would lose the element type),
    /// and a defined element is tagged with it, so the method grows it through
    /// its own type check.
    // Cost: O(1) for an untyped or non-container element; a cell store for a
    // typed hole.
    fn vivify_typed_hole(&mut self, cell: &Value) -> Result<(), RuntimeError> {
        let constraint = match cell.view() {
            ValueView::ContainerRef(c) => crate::value::lookup_cell_constraint(&c).map(|c| c.ty),
            ValueView::HashEntryRef { .. } => cell
                .hash_entry_locate()
                .and_then(|t| t.element_constraint())
                .map(|(ty, _)| ty),
            _ => None,
        };
        let Some(ty) = constraint else {
            return Ok(());
        };
        if !ty.starts_with("Array[") && !ty.starts_with("Hash[") {
            return Ok(());
        }
        let current = cell.deref_container();
        match current.descalarize().view() {
            ValueView::Array(..) | ValueView::Hash(..) => {
                Self::tag_element_value_type_in_place(current.descalarize(), &ty);
            }
            ValueView::Nil | ValueView::Package(_) => {
                let fresh = if ty.starts_with("Array[") {
                    Value::real_array(Vec::new())
                } else {
                    Value::hash(crate::value::ValueMap::default())
                };
                Self::tag_element_value_type_in_place(&fresh, &ty);
                if let Some(stored) = self.assign_lvalue_container(cell, fresh) {
                    stored?;
                }
            }
            _ => {}
        }
        Ok(())
    }

    /// An element location produced for a Junction-threaded vivifying call:
    /// an undefined element is replaced by a fresh `Array` stored through the
    /// location, and the element's container node is answered.
    // Cost: O(1) for a cell store.
    fn vivify_element_array(&mut self, element: Value) -> Result<Value, RuntimeError> {
        if !element.is_container_ref() && !matches!(element.view(), ValueView::HashEntryRef { .. })
        {
            return Ok(element);
        }
        let current = element.deref_container();
        let undefined = matches!(current.view(), ValueView::Nil)
            || matches!(current.view(), ValueView::Package(p) if p == "Any" || p == "Mu");
        if !undefined {
            return Ok(current);
        }
        let fresh = Value::real_array(Vec::new());
        if let Some(stored) = self.assign_lvalue_container(&element, fresh.clone()) {
            stored?;
        }
        Ok(fresh)
    }

    /// A raw `VMArray`/`VMHash` element read through a Raku subscript comes
    /// back hllized, as Rakudo's `AT-POS`/`AT-KEY` hand it over (ADR-12666).
    // Cost: O(1) (one tag probe).
    #[inline]
    pub(crate) fn hllize_vm_collection_on_top(&mut self) {
        if let Some(raw) = self.stack.pop_if(|top| top.is_vm_collection()) {
            self.stack.push(raw.hllize_vm_collection());
        }
    }

    /// `Index` in ARGUMENT position ([`OpCode::IndexArgRef`]). Produces the
    /// element's container when the callee this argument is being compiled for
    /// binds it to the caller's location, and is `Index` otherwise.
    ///
    /// The producer is the same one the receiver half uses, so `$b(@a[0])` and
    /// `@a[0].mut` promote the same slot to the same cell; only the gate
    /// differs, because rawness here is a property of the *callee's signature*
    /// (or of its bare-block-ness) rather than of the element's type.
    pub(crate) fn exec_index_arg_ref_op(
        &mut self,
        code: &CompiledCode,
        mark: &crate::opcode::IndexArgRefMark,
    ) -> Result<(), RuntimeError> {
        if self.index_arg_callee_binds_container(code, mark)
            && let Some(cell) = self
                .take_subscript_element_cell(mark.is_positional, true)
                .or_else(|| self.take_subscript_slice_cells(mark.is_positional))
        {
            // Scoped to ONE subscript dispatch; consume it here too so producing
            // a cell instead cannot leak the suppression onto the next one.
            self.dispatch.skip_postcircumfix_overload = false;
            self.stack.push(cell);
            return Ok(());
        }
        // A List item that is the cell of a native variable stays a container
        // argument whatever the callee binds, so a multi dispatch sees the
        // `byte` of `($b, $b)[0]` as rakudo does; any other element is read as
        // a value (ADR-15309).
        if mark.is_positional
            && let Some(cell) = self.list_native_item_cell()
        {
            self.stack.pop();
            self.stack.pop();
            self.stack.push(cell);
            return Ok(());
        }
        // A `Buf` element passed to a routine with a native parameter at that
        // position is a native `uint*` container for dispatch, as in rakudo
        // (`m($buf[0])` picks `multi m(byte)`; a `Blob` element stays `Int`):
        // the value is read as usual and boxed in a native-typed cell.
        let buf_native = if mark.is_positional {
            self.buf_item_native_type(code, mark)
        } else {
            None
        };
        // A callee that does not bind the container reads the value, exactly
        // as the plain `Index` op does.
        self.exec_index_op_with_positional(mark.is_positional)?;
        self.decont_native_pos_ref_on_top()?;
        if let Some(native) = buf_native
            && let Some(top) = self.stack.last()
        {
            let native_cell = |value: Value| {
                let cell = crate::gc::Gc::new(crate::value::ContainerCell::new(value));
                crate::value::register_container_constraint(&cell, native);
                Value::container_ref(cell)
            };
            match top.view() {
                ValueView::Int(_) => {
                    let value = self.stack.pop().unwrap_or(Value::NIL);
                    self.stack.push(native_cell(value));
                }
                // A slice (`$buf[2..5]`) is a List of the selected elements,
                // each a native container (#15858): the callee's `@a[0]` then
                // hands the cell on, as for a List literal of native variables.
                ValueView::Array(items, crate::value::ArrayKind::List)
                    if items.iter().all(|v| matches!(v.view(), ValueView::Int(_))) =>
                {
                    let cells: Vec<Value> = items.iter().cloned().map(native_cell).collect();
                    self.stack.pop();
                    self.stack.push(Value::array(cells));
                }
                _ => {}
            }
        }
        Ok(())
    }

    /// The native element type of the `Buf` the subscript on top of the stack
    /// reads (`uint8` for a `buf8`), when the callee has a native-typed
    /// parameter at this argument's position; `None` otherwise, and for a `Blob`
    /// (whose elements rakudo dispatches as boxed `Int`).
    // Cost: O(1) for a non-Buf receiver (one tag probe); a Buf receiver walks
    // the callee's candidates once.
    fn buf_item_native_type(
        &mut self,
        code: &CompiledCode,
        mark: &crate::opcode::IndexArgRefMark,
    ) -> Option<&'static str> {
        let n = self.stack.len();
        let target = self.stack.get(n.checked_sub(2)?)?;
        let ValueView::Instance { class_name, .. } = target.view() else {
            return None;
        };
        let name = class_name.resolve();
        let native = match name.as_str() {
            "buf8" | "Buf[uint8]" | "Buf[byte]" => "uint8",
            "buf16" | "Buf[uint16]" => "uint16",
            "buf32" | "Buf[uint32]" => "uint32",
            "buf64" | "Buf[uint64]" => "uint64",
            _ => return None,
        };
        let crate::opcode::RwArgCallee::Named { name_idx } = &mark.mark.callee else {
            return None;
        };
        let routine = Self::const_str(code, *name_idx);
        let routine_sym = code.const_sym(*name_idx);
        let positional = mark.mark.positional as usize;
        self.routine_takes_native_at(routine, routine_sym, positional)
            .then_some(native)
    }

    /// The cell a `List` (or a scalar holding one) already stores at the
    /// integer index on top of the stack, when that cell is native-typed;
    /// `None` for every other shape. Never promotes a slot: an `Array`'s slots
    /// are fresh `Scalar`s, which rakudo does not dispatch as native.
    /// Leaves the stack alone.
    // Cost: O(1): two tag probes and one slot read.
    fn list_native_item_cell(&self) -> Option<Value> {
        let n = self.stack.len();
        if n < 2 {
            return None;
        }
        let ValueView::Int(i) = self.stack[n - 1].view() else {
            return None;
        };
        let i = usize::try_from(i).ok()?;
        let target = &self.stack[n - 2];
        let held;
        let target = if target.is_container_ref() {
            held = target.deref_container();
            &held
        } else {
            target
        };
        let ValueView::Array(
            items,
            crate::value::ArrayKind::List | crate::value::ArrayKind::ItemList,
        ) = target.view()
        else {
            return None;
        };
        let slot = items.get(i)?;
        slot.native_container_type().is_some().then(|| slot.clone())
    }

    /// `$native-array[$i]` answers the element's reference (`IntPosRef`, what
    /// upstream's typed `CArray` returns from `AT-POS ... is raw`). A plain
    /// read wants the value in it: a number handed to `^`, `x`, `..`, a
    /// subscript or `-` is read by pure Rust that cannot FETCH. The forms that
    /// want the container -- a bind, an `is rw` argument, `.VAR` -- are other
    /// opcodes (`IndexVarRef`, `IndexArgRef` toward a binding callee, ...).
    // Cost: O(1) (one tag probe; one element decode for a reference).
    pub(crate) fn decont_native_pos_ref_on_top(&mut self) -> Result<(), RuntimeError> {
        self.hllize_vm_collection_on_top();
        if let Some(reference) = self
            .stack
            .pop_if(|top| crate::runtime::native_pos_ref::is_native_pos_ref(top))
        {
            let value = loan_env!(self, auto_fetch_proxy(&reference))?;
            self.stack.push(value);
        }
        Ok(())
    }

    /// The element cell for a subscript receiver, or `None` for every shape
    /// that is not a direct hit on an existing element of a real `Array`/`Hash`.
    ///
    /// Declining leaves the stack **untouched**, so the caller runs the ordinary
    /// `Index` op verbatim: slices, `Whatever`, junction receivers, `Range`
    /// targets, `postcircumfix` overloads and every other shape stay on the one
    /// implementation that already handles them, rather than being partially
    /// re-derived here.
    ///
    /// `argument` is the ARGUMENT producer's own rule (see
    /// [`Self::exec_index_arg_ref_op`]): an argument position binds the
    /// element's *location*, so a subscript past the end of an array, or a
    /// missing hash key, hands over the deferred vivification token
    /// (`HashEntryRef`) instead of declining. The token creates the element on
    /// the first write through the parameter and never before, which is the
    /// read-safety ADR-0059 Slice 3 needed: `sub f(\x) { x }; f(@a[5])` leaves
    /// `@a` as it was, while `sub g($x is rw) { $x = 9 }; g(@a[5])` grows it
    /// (rakudo: `[1 2 (Any) (Any) (Any) 9]`). A receiver (`@a[5].mut`) is not
    /// a bind and keeps declining.
    fn take_subscript_element_cell(
        &mut self,
        is_positional: bool,
        argument: bool,
    ) -> Option<Value> {
        let n = self.stack.len();
        if n < 2 {
            return None;
        }
        let index = self.stack[n - 1].clone();
        let target = self.stack[n - 2].clone();
        let cell = self.subscript_element_cell(&target, &index, is_positional, argument)?;
        self.stack.pop();
        self.stack.pop();
        Some(cell)
    }

    /// The location `target[index]` (`target{index}` unless `is_positional`)
    /// names, for [`Self::take_subscript_element_cell`]; `None` for every
    /// shape it does not hand out (see there). Leaves the stack alone.
    // Cost: O(1) amortized: one slot promotion, or one hash probe.
    fn subscript_element_cell(
        &mut self,
        target: &Value,
        index: &Value,
        is_positional: bool,
        argument: bool,
    ) -> Option<Value> {
        let cell = if is_positional {
            // Only a *mutable* array has element locations to hand out. A
            // `List`/`ItemList` element is immutable in raku (`(1,2)[0].mut`
            // dies), so promoting one to a cell would turn a refusal into a
            // silent success — worse than the silent no-op it is today. `Lazy`
            // is excluded because promoting an element would reify it.
            let len = match target.view() {
                ValueView::Array(
                    items,
                    crate::value::ArrayKind::Array
                    | crate::value::ArrayKind::ItemArray
                    | crate::value::ArrayKind::Shaped,
                ) => items.len(),
                _ => return None,
            };
            // `@a[*-1].mut` addresses one element like any other subscript, so
            // the `*` is resolved here the same way the read path resolves it.
            let index = self
                .eval_whatever_code_index(index, len as i64)
                .unwrap_or_else(|| index.clone());
            let ValueView::Int(i) = index.view() else {
                return None;
            };
            if i < 0 {
                return None;
            }
            if !argument && i as usize >= len {
                return None;
            }
            // `terminal: true`: past the end this is the deferred token, never
            // an eager growth (see the doc comment).
            target.array_slot_ref(i as usize, true)?
        } else {
            // An object hash (`my %h{Any}`) stores `.WHICH`-encoded keys, so the
            // subscript has to be encoded the same way the read path encodes it.
            let object_hash = match target.view() {
                ValueView::Hash(map) => map.key_type.is_some(),
                _ => return None,
            };
            // A list or range index is a slice (`%h<a c>`), not one key: it
            // must not be encoded into a single `"a c"` key.
            if matches!(
                index.view(),
                ValueView::Array(..)
                    | ValueView::Seq(..)
                    | ValueView::Range(..)
                    | ValueView::RangeExcl(..)
                    | ValueView::RangeExclStart(..)
                    | ValueView::RangeExclBoth(..)
                    | ValueView::GenericRange { .. }
            ) && !object_hash
            {
                return None;
            }
            let key = if object_hash {
                crate::runtime::utils::value_which_key(index)
            } else {
                Value::hash_key_encode(index)
            };
            // An object hash remembers the key OBJECT beside its `.WHICH` string;
            // the deferred token writes only the string, so register the object
            // now (`my Array %h{K}; %h{$k}.push(1)` must leave a `K` key).
            if argument && object_hash {
                let mut hash = target.clone();
                hash.with_hash_mut(|gc| {
                    let data = crate::value::gc_data_mut(gc);
                    if !data.map.contains_key(key.as_str()) {
                        data.original_keys
                            .get_or_insert_with(crate::value::ValueMap::default)
                            .insert(key.clone(), index.clone());
                    }
                });
            }
            // A MISSING key hands back the deferred token, exactly like the
            // array arm past the end.
            target.hash_slot_ref(&key, true)?
        };
        match cell.view() {
            ValueView::ContainerRef(_) => {}
            // The deferred token is a location only for an argument; for a
            // receiver it is a read.
            ValueView::HashEntryRef { .. } if argument => {}
            _ => return None,
        }
        Some(cell)
    }

    /// A slice argument (`f(@a[0, 2])`, `f(%h<a c>)`) toward a callee that
    /// binds its argument's location: the `List` of the selected elements'
    /// locations, flagged as a bound slice ([`Value::bound_slice_array`]) so a
    /// whole-value assignment through the parameter distributes over them, as
    /// Rakudo's slice of element containers does (#13786). An empty selection
    /// is a bound slice too, recognised by its flag rather than by its
    /// contents. `None`, with the stack untouched, unless the subscript is a
    /// finite list of plain indices or keys into a real `Array`/`Hash` (a
    /// `Whatever`, a lazy or junction index and every other shape keep the
    /// ordinary `Index` read).
    // Cost: O(k) amortized, k = selected elements.
    fn take_subscript_slice_cells(&mut self, is_positional: bool) -> Option<Value> {
        let n = self.stack.len();
        if n < 2 {
            return None;
        }
        let index = self.stack[n - 1].clone();
        let target = self.stack[n - 2].clone();
        let keys: Vec<Value> = match index.view() {
            ValueView::Array(items, _) => items.to_vec(),
            ValueView::Range(..)
            | ValueView::RangeExcl(..)
            | ValueView::RangeExclStart(..)
            | ValueView::RangeExclBoth(..)
                if !crate::value::flat::is_infinite_range(&index) =>
            {
                crate::runtime::utils::value_to_list(&index)
            }
            _ => return None,
        };
        let mut cells = Vec::with_capacity(keys.len());
        for key in &keys {
            // A nested list (`@a[(0, 1), 2]`) or a `Whatever` element is a
            // multi-level selection this producer does not flatten.
            if matches!(key.view(), ValueView::Array(..) | ValueView::Whatever) {
                return None;
            }
            cells.push(self.subscript_element_cell(&target, key, is_positional, true)?);
        }
        self.stack.pop();
        self.stack.pop();
        Some(Value::bound_slice_array(cells))
    }

    /// Arm slice 3b's arrival channel for a `CallMethod` whose receiver is
    /// already a container — which, after the producer above, is what a
    /// subscript receiver is.
    ///
    /// `CallMethodMut`'s arming site starts from a *name*; this one starts from
    /// the location itself, so it needs neither `capture_lvalue_invocant_cell`
    /// nor a receiver name. The oracle it consults, and the re-check the binder
    /// performs before consuming the channel, are slice 3b's unchanged.
    ///
    /// `receiver_idx` and `args_from` are stack positions rather than an arity
    /// because the two dispatch opcodes lay the frame out differently:
    /// `CallMethod` is `[receiver, args..]` while `CallMethodDynamic` puts the
    /// runtime method name between them.
    pub(super) fn arm_raw_invocant_arrival_from_receiver(
        &mut self,
        receiver_idx: usize,
        args_from: usize,
        method: &str,
    ) -> bool {
        if !crate::runtime::raw_invocant::any_raw_invocant_method_possible() {
            return false;
        }
        // Both call sites derive these with `checked_sub` against the same
        // stack, so they are in range; read them fallibly anyway rather than let
        // a future caller's arithmetic turn into a panic.
        let Some(target) = self
            .stack
            .get(receiver_idx)
            .filter(|t| t.is_container_ref())
        else {
            return false;
        };
        let target = target.clone();
        let Some(args) = self.stack.get(args_from..).map(<[Value]>::to_vec) else {
            return false;
        };
        // The callee is resolved against the *contained* value: the container is
        // transport, and `@a[0]`'s element is what carries the type whose method
        // is about to run.
        let inner = target.deref_container();
        if !self.method_binds_raw_invocant(&inner, method, &args) {
            return false;
        }
        self.pending_raw_invocant = Some(Box::new(
            crate::vm::vm_raw_invocant_arrival::PendingRawInvocant {
                method: method.to_string(),
                cell: target,
                raw_parameter: true,
                implicit_self: false,
            },
        ));
        true
    }
}
