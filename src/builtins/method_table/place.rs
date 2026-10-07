//! What a receiver-mutating row writes through (ADR-11276 §8.1, decided in
//! slice 3F, §9.23).
//!
//! Container identity (ADR-0013 §3) already made an array, hash or quant-hash
//! mutator write the container's shared `Gc` node in place, so every holder
//! sees the change. The place is for the rest of what a mutator needs from its
//! receiver:
//!
//! - **a name**, because the declared element type, `is default`, the
//!   thread-shared store, a compunit's unit-lexical cell, an `our` package
//!   array and a `:=`-bound cell are all found by the variable's name
//!   (`Interpreter::env_root_descended_mut`);
//! - **a replacement**, for a method that changes the value rather than the
//!   container it is in (`Str.subst-mutate`);
//! - **a detached container**, for a receiver with no name at all (the backing
//!   array of an `is Array` instance, `f().push(1)`).
//!
//! When ADR-0097's binding descriptor lands, the accessors here change and the
//! handlers do not.

use crate::opcode::CompiledCode;
use crate::runtime::Interpreter;
use crate::value::Value;

/// The receiver of a [`Handler::Mut`](super::Handler::Mut) row.
pub(crate) enum ReceiverPlace<'a> {
    /// The call named a binding: `@a.push`, `$r.splice` where `$r` holds an
    /// array, `%h.push`, `@!items.pop`. `value` is the receiver as the call
    /// read it. `code` is the VM chunk whose local slots mirror the binding, so
    /// a write reaches both halves of the dual store; it is `None` when the
    /// call did not come through the VM. An empty `name` is a receiver that
    /// has none (the VM passes `""` for `$obj.bag.add(...)`), which behaves as
    /// a detached one.
    Var {
        name: &'a str,
        value: &'a Value,
        code: Option<&'a CompiledCode>,
    },
    /// A container with no binding: an `is Array` instance's storage, a
    /// by-value receiver (`f().push(1)`, `[1, 2].pop`).
    Detached(&'a mut Value),
}

impl<'a> ReceiverPlace<'a> {
    /// A binding named `name` whose value the call read as `value`, for a call
    /// that came through the VM, whose chunk `code` maps `name` to a local slot.
    // Cost: O(1).
    pub(crate) fn var_in(name: &'a str, value: &'a Value, code: &'a CompiledCode) -> Self {
        ReceiverPlace::Var {
            name,
            value,
            code: Some(code),
        }
    }

    /// A container with no binding.
    // Cost: O(1).
    pub(crate) fn detached(value: &'a mut Value) -> Self {
        ReceiverPlace::Detached(value)
    }

    /// The receiver as the call read it.
    // Cost: O(1).
    pub(crate) fn value(&self) -> &Value {
        match self {
            ReceiverPlace::Var { value, .. } => value,
            ReceiverPlace::Detached(value) => value,
        }
    }

    /// Re-seat the receiver, already mutated through its shared node, in both
    /// halves of the dual store (the env entry and the VM's local slot), so a
    /// later locals-to-env sync cannot resurrect a stale snapshot of it. A
    /// receiver with no name has nothing to re-seat.
    // Cost: O(1) for the env entry, O(l) to find the local slot by name, l =
    // local slots of the chunk.
    pub(crate) fn reseat(&self, interp: &mut Interpreter) {
        let ReceiverPlace::Var {
            name, value, code, ..
        } = self
        else {
            return;
        };
        if name.is_empty() {
            return;
        }
        interp
            .env_mut()
            .insert((*name).to_string(), (*value).clone());
        if let Some(code) = code {
            interp.update_local_if_exists(code, name, value);
        }
    }
}
