//! ADR-15829: the unquoted `.WHAT` / `.HOW` lane of `CallMethod` / `CallMethodMut`.
//!
//! `.WHAT` and `.HOW` are macros in Rakudo: an unquoted call cannot reach a
//! user method (`builtin_shadow::is_mop_pseudo_method`), so for a receiver
//! that needs no container, Proxy or user-class handling the whole chain in
//! front of `dispatch_what` / `dispatch_how` (`call_method_with_values_inner`
//! -> `try_native_method` -> `dispatch_method_by_name_1`, ~25 functions, 36k /
//! 69k Ir per call) has exactly one answer, the one those functions give.
//!
//! This lane asks them directly when the call is an unquoted, argument-less
//! `.WHAT` / `.HOW` and the receiver is a plain scalar value (`Int`, `Num`,
//! `Str`, `Bool`), a type object (`Package`), or a collection or number value
//! that carries its own type (`Hash`, `Array`, `Pair`, `Rat`, a `Range`, ...;
//! see `pseudo_lane_value_receiver`, `.WHAT` only; #17530: EEL's `$v.WHAT === Int || ...` test
//! on a `Hash` cost ~31k instructions per `.WHAT`). Every other call -- quoted, a
//! modifier, arguments, an accessor-ref marker, a wrapped method, any other
//! receiver (a container, a Proxy, an Instance, a Failure, ...) -- falls
//! through to the generic path unchanged, which is the miss tail.
//!
//! The ADR calls this an opcode; a lane at the top of the two existing handlers
//! has the same guard and the same miss tail, and the handlers already carry
//! the call-shape facts (`quoted`, `modifier_idx`, `arg_sources_idx`) the guard
//! needs, so it needs no new `OpCode` arm or compiler change.

use super::*;

#[derive(Clone, Copy, PartialEq, Eq)]
enum PseudoKind {
    What,
    How,
    Var,
}

impl Interpreter {
    /// Answer an unquoted, argument-less `.WHAT` / `.HOW` on a plain scalar or
    /// a type object, popping the receiver off the stack. `None` leaves the
    /// stack untouched and the call to the generic path.
    // Cost: O(1) for the guard (one name compare, one receiver tag test);
    // a hit costs `dispatch_what` / `dispatch_how` and nothing in front of them.
    #[inline]
    pub(super) fn try_pseudo_method_lane(
        &mut self,
        method: &str,
        arity: u32,
        modifier_idx: Option<u32>,
        quoted: bool,
        arg_sources_idx: Option<u32>,
    ) -> Option<Result<Value, RuntimeError>> {
        if arity != 0
            || modifier_idx.is_some()
            || quoted
            || arg_sources_idx.is_some()
            || self.accessor_ref_pending
        {
            return None;
        }
        let kind = match method {
            "WHAT" => PseudoKind::What,
            "HOW" => PseudoKind::How,
            "VAR" => PseudoKind::Var,
            _ => return None,
        };
        let target = self.stack.last()?;
        // `.VAR` on a `Proxy` (or a subclass of it): the Proxy itself,
        // decontainerized, so the next method call does not FETCH. Rakudo
        // compiles `.VAR` itself, so a `method VAR` a Proxy subclass declares is
        // never called there either.
        if kind == PseudoKind::Var {
            if !matches!(target.view(), ValueView::Proxy { .. })
                || self.registry().method_name_ever_wrapped(method)
            {
                return None;
            }
            let target = self.stack.pop()?;
            self.pending_call_arg_source_slots.clear();
            self.set_pending_call_arg_sources(None);
            crate::vm::vm_stats::record_dispatch_entry_outcome("callmethod", "pseudo-lane");
            return Some(Ok(Value::proxy_var_object(target, String::new())));
        }
        if !Self::pseudo_lane_scalar_receiver(target)
            && (kind != PseudoKind::What || !Self::pseudo_lane_value_receiver(target))
        {
            return None;
        }
        // A `BOOT*` type object (and a raw VM collection) answers its meta
        // questions itself (`vm_collection_method`: `nqp::bootint().HOW` is
        // `KnowHOW`), ahead of `dispatch_how`.
        if crate::runtime::boot_collection::is_boot_collection_invocant(target) {
            return None;
        }
        // A wrap of this method could intercept the call on the generic path
        // (a wrap of any other method cannot).
        if self.registry().method_name_ever_wrapped(method) {
            return None;
        }
        let target = target.clone();
        // The bookkeeping the full path's argument decode performs for a call
        // without argument sources.
        self.pending_call_arg_source_slots.clear();
        self.set_pending_call_arg_sources(None);
        self.stack.pop();
        crate::vm::vm_stats::record_dispatch_entry_outcome("callmethod", "pseudo-lane");
        Some(if kind == PseudoKind::What {
            self.dispatch_what(&target, Vec::new())
        } else {
            self.dispatch_how(&target, &[])
        })
    }

    /// A plain scalar or a type object: its `.WHAT` and `.HOW` are answered by
    /// `dispatch_what` / `dispatch_how` on the value alone.
    // Cost: O(1), one tag test.
    fn pseudo_lane_scalar_receiver(target: &Value) -> bool {
        matches!(
            target.view(),
            ValueView::Int(_)
                | ValueView::BigInt(_)
                | ValueView::Num(_)
                | ValueView::Str(_)
                | ValueView::Bool(_)
                | ValueView::Package(_)
        )
    }

    /// A collection or number value whose `.WHAT` the generic path answers with
    /// `dispatch_what` on the value itself, with nothing in front of it that
    /// reads anything but the value (`Hash`, `Array`/`List`, `Pair`, `Rat`,
    /// `Complex`, a `Range`, a `QuantHash`). A typed or shaped collection
    /// carries its type on the value (`container_type_metadata`), which
    /// `dispatch_what` reads. `.HOW` is left to the generic path for these: a
    /// typed collection's meta-object is built there.
    ///
    /// Not admitted, because the generic path does something to them first:
    /// a container or `.VAR` view (decontainerized, or answered as `Scalar`),
    /// a `Proxy` (FETCHed), a lazy list or `Seq` (the read paths that reify
    /// it), a `Junction`, an `Instance` (mixins, user classes; the
    /// `CallMethodMut` MOP lane covers those), a `Failure`, and any other kind.
    // Cost: O(1), one tag test.
    fn pseudo_lane_value_receiver(target: &Value) -> bool {
        matches!(
            target.view(),
            ValueView::Hash(_)
                | ValueView::Array(..)
                | ValueView::Pair(..)
                | ValueView::ValuePair(..)
                | ValueView::Rat(..)
                | ValueView::FatRat(..)
                | ValueView::BigRat(..)
                | ValueView::Complex(..)
                | ValueView::Range(..)
                | ValueView::RangeExcl(..)
                | ValueView::RangeExclStart(..)
                | ValueView::RangeExclBoth(..)
                | ValueView::Set(..)
                | ValueView::Bag(..)
                | ValueView::Mix(..)
        )
    }
}
