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
    ) -> Result<(), RuntimeError> {
        if crate::runtime::raw_invocant::any_raw_invocant_method_possible()
            && let Some(cell) = self.take_subscript_element_cell(is_positional, false)
        {
            // `skip_postcircumfix_overload` is scoped to ONE subscript dispatch
            // and is consumed by `exec_index_op_with_positional`; consume it
            // here too so producing a cell instead cannot leak the suppression
            // onto the next, unrelated subscript. (An overload can only be
            // declared for an `Instance` receiver, which the producer declines,
            // so this is hygiene rather than a reachable bug today.)
            self.skip_postcircumfix_overload = false;
            self.stack.push(cell);
            return Ok(());
        }
        self.exec_index_op_with_positional(is_positional)
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
            && let Some(cell) = self.take_subscript_element_cell(mark.is_positional, true)
        {
            // Scoped to ONE subscript dispatch; consume it here too so producing
            // a cell instead cannot leak the suppression onto the next one.
            self.skip_postcircumfix_overload = false;
            self.stack.push(cell);
            return Ok(());
        }
        self.exec_index_op_with_positional(mark.is_positional)
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
    /// `grow` is the ARGUMENT producer's own rule (see
    /// [`Self::exec_index_arg_ref_op`]): an argument position is a definite
    /// bind, so a subscript past the end vivifies the element rather than
    /// declining. A receiver (`@a[5].mut`) is not a bind and keeps the
    /// declining behaviour, which is why the two producers no longer share one
    /// answer here.
    fn take_subscript_element_cell(&mut self, is_positional: bool, grow: bool) -> Option<Value> {
        let n = self.stack.len();
        if n < 2 {
            return None;
        }
        let index = self.stack[n - 1].clone();
        let target = self.stack[n - 2].clone();
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
                .eval_whatever_code_index(&index, len as i64)
                .unwrap_or(index);
            let ValueView::Int(i) = index.view() else {
                return None;
            };
            if i < 0 {
                return None;
            }
            // Past the end, `terminal: true` hands back a deferred
            // vivification token rather than a location -- right for
            // `my $r := @a[5]`, and right for `@a[5].mut`, which has no
            // element to hand over. An ARGUMENT is a definite bind, so it asks
            // for the eager (`terminal: false`) growth instead: rakudo answers
            // `my @a = 1, 2; $r(@a[5])` with `[1 2 (Any) (Any) (Any) 9]`, and
            // so does mutsu's own NAMED-callee path through `CallFunc`'s
            // copy-in/copy-out temp protocol.
            if !grow && i as usize >= len {
                return None;
            }
            // `terminal` only decides the past-the-end behaviour, and an
            // in-range index never reaches it, so `!grow` says exactly "defer
            // unless this is an argument".
            target.array_slot_ref(i as usize, !grow)?
        } else {
            // An object hash (`my %h{Any}`) stores `.WHICH`-encoded keys, so the
            // subscript has to be encoded the same way the read path encodes it.
            let object_hash = match target.view() {
                ValueView::Hash(map) => map.key_type.is_some(),
                _ => return None,
            };
            let key = if object_hash {
                crate::runtime::utils::value_which_key(&index)
            } else {
                Value::hash_key_encode(&index)
            };
            let cell = target.hash_slot_ref(&key, true)?;
            // Same rule on the associative side: a MISSING key hands back the
            // deferred token, which is right for a receiver but not for an
            // argument -- a definite bind vivifies (`$r(%h<k>)` leaves
            // `{:k(9)}` in rakudo). `terminal` cannot express that here (a
            // missing key defers either way), so the entry is created and the
            // slot re-taken. An EXISTING entry is untouched, which keeps the
            // `terminal: true` promotion of a nested Array/Hash element.
            if grow && !matches!(cell.view(), ValueView::ContainerRef(_)) {
                target.hash_assign_at(&key, Value::NIL)?;
                target.hash_slot_ref(&key, true)?
            } else {
                cell
            }
        };
        // A missing key hands back a lazy `HashEntryRef` token rather than a
        // location; that is a read, not a receiver container.
        if !matches!(cell.view(), ValueView::ContainerRef(_)) {
            return None;
        }
        self.stack.pop();
        self.stack.pop();
        Some(cell)
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
            },
        ));
        true
    }

    /// The `CallMethodDynamic` spelling (`@a[0]."$name"()`), whose stack frame
    /// is `[receiver, method-name, args..]`. The name is a runtime value, so
    /// the gate reads it off the stack rather than from the constant pool.
    pub(super) fn arm_raw_invocant_arrival_from_dynamic_receiver(
        &mut self,
        arity: u32,
        modifier: Option<&str>,
    ) -> bool {
        if !crate::runtime::raw_invocant::any_raw_invocant_method_possible() {
            return false;
        }
        let Some(receiver_idx) = self.stack.len().checked_sub(arity as usize + 2) else {
            return false;
        };
        let name_val = self.stack[receiver_idx + 1].clone();
        let method = Self::rewrite_method_name(&Self::dynamic_method_name(&name_val), modifier);
        self.arm_raw_invocant_arrival_from_receiver(receiver_idx, receiver_idx + 2, &method)
    }
}
