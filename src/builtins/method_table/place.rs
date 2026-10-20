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
        /// The caller's variable behind each positional argument (`Some("$x")`
        /// for `@a.BIND-POS(0, $x)`, `None` for a literal or an expression), as
        /// the call site wrote them. Only a row that binds an element to the
        /// caller's *variable* reads them ([`Self::arg_source`]); empty when the
        /// call carried none.
        arg_sources: &'a [Option<String>],
    },
    /// A container with no binding: an `is Array` instance's storage, a
    /// by-value receiver (`f().push(1)`, `[1, 2].pop`).
    Detached(&'a mut Value),
}

impl<'a> ReceiverPlace<'a> {
    /// A binding named `name` whose value the call read as `value`, for a call
    /// that did not come through the VM (the interpreter's by-name entries).
    // Cost: O(1).
    pub(crate) fn var(name: &'a str, value: &'a Value) -> Self {
        ReceiverPlace::Var {
            name,
            value,
            code: None,
            arg_sources: &[],
        }
    }

    /// A binding named `name` whose value the call read as `value`, for a call
    /// that came through the VM, whose chunk `code` maps `name` to a local slot.
    // Cost: O(1).
    pub(crate) fn var_in(name: &'a str, value: &'a Value, code: &'a CompiledCode) -> Self {
        ReceiverPlace::Var {
            name,
            value,
            code: Some(code),
            arg_sources: &[],
        }
    }

    /// The same binding, read as `value` now: the place a row hands on after
    /// it replaced what the binding holds (`Any.push` autovivifying it to an
    /// `Array`). `None` for a detached receiver, which has no binding to
    /// re-read.
    // Cost: O(1).
    pub(crate) fn rebound<'b>(&'b self, value: &'b Value) -> Option<ReceiverPlace<'b>> {
        match self {
            ReceiverPlace::Var {
                name,
                code,
                arg_sources,
                ..
            } => Some(ReceiverPlace::Var {
                name,
                value,
                code: *code,
                arg_sources,
            }),
            ReceiverPlace::Detached(_) => None,
        }
    }

    /// The same place, carrying the call's argument sources (see
    /// [`ReceiverPlace::Var`]). A detached place has no caller variables, so
    /// it is returned unchanged.
    // Cost: O(1).
    pub(crate) fn with_arg_sources(mut self, sources: &'a [Option<String>]) -> Self {
        if let ReceiverPlace::Var { arg_sources, .. } = &mut self {
            *arg_sources = sources;
        }
        self
    }

    /// Whether the call came through the VM (its chunk maps names to local
    /// slots), the only entry that carries argument sources.
    // Cost: O(1).
    pub(crate) fn came_through_vm(&self) -> bool {
        matches!(self, ReceiverPlace::Var { code: Some(_), .. })
    }

    /// The caller's variable that positional argument `index` was written as,
    /// if the call site named one.
    // Cost: O(1).
    pub(crate) fn arg_source(&self, index: usize) -> Option<&str> {
        match self {
            ReceiverPlace::Var { arg_sources, .. } => {
                arg_sources.get(index).and_then(|s| s.as_deref())
            }
            ReceiverPlace::Detached(_) => None,
        }
    }

    /// Install `cell` as the container of the caller's variable `name`, in both
    /// halves of the dual store: a bind to an element promotes the source
    /// variable into the shared cell, so a later `$x = ...` writes through to
    /// the element and vice versa.
    // Cost: O(1) for the env entry, O(l) to find the local slot by name, l =
    // local slots of the chunk.
    pub(crate) fn install_source_cell(&self, interp: &mut Interpreter, name: &str, cell: Value) {
        interp.set_env_with_main_alias(name, cell.clone());
        if let ReceiverPlace::Var {
            code: Some(code), ..
        } = self
        {
            interp.update_local_if_exists(code, name, &cell);
        }
    }

    /// A container with no binding.
    // Cost: O(1).
    pub(crate) fn detached(value: &'a mut Value) -> Self {
        ReceiverPlace::Detached(value)
    }

    /// The binding's name, if the receiver has one.
    // Cost: O(1).
    pub(crate) fn name(&self) -> Option<&str> {
        match self {
            ReceiverPlace::Var { name, .. } if !name.is_empty() => Some(name),
            _ => None,
        }
    }

    /// The VM chunk whose local slots mirror the binding, when the call came
    /// through the VM.
    // Cost: O(1).
    pub(crate) fn code(&self) -> Option<&'a CompiledCode> {
        match self {
            ReceiverPlace::Var { code, .. } => *code,
            ReceiverPlace::Detached(_) => None,
        }
    }

    /// The receiver as the call read it. It is a snapshot: [`Self::assign`]
    /// does not update it.
    // Cost: O(1).
    pub(crate) fn value(&self) -> &Value {
        match self {
            ReceiverPlace::Var { value, .. } => value,
            ReceiverPlace::Detached(value) => value,
        }
    }

    /// The live container to write through: what the binding's name resolves to
    /// now (the interpreter's env helper finds a compunit's unit-lexical cell
    /// or an `our` package container and descends a `:=`-bound cell), or the
    /// detached value itself. `None` when a binding's name no longer resolves,
    /// which a handler answers by declining or by rebuilding from
    /// [`Self::value`].
    // Cost: O(1) for a detached value; for a binding, one env lookup plus the
    // cell descent.
    pub(crate) fn slot<'s>(&'s mut self, interp: &'s mut Interpreter) -> Option<&'s mut Value> {
        match self {
            ReceiverPlace::Var { name, .. } if !name.is_empty() => {
                interp.env_root_descended_mut(name)
            }
            ReceiverPlace::Var { .. } => None,
            ReceiverPlace::Detached(value) => Some(&mut **value),
        }
    }

    /// For a binding reached through a `:=`-bound cell: the address of the
    /// outermost cell, the identity two threads sharing the container have in
    /// common (ADR-0068 §2), and the container the cell holds *now*, which is
    /// what the row will mutate (it can differ from [`Self::value`], read
    /// before another thread re-seated the cell). `None` for a binding that is
    /// not celled and for a detached receiver.
    // Cost: one env lookup plus the cell descent, as [`Self::slot`].
    pub(crate) fn binding_cell(&self, interp: &mut Interpreter) -> Option<(usize, Value)> {
        let ReceiverPlace::Var { name, .. } = self else {
            return None;
        };
        if name.is_empty() {
            return None;
        }
        let mut addr = None;
        let slot = std::ptr::from_mut(interp.env_root_descended_mut_tracked(name, &mut addr)?);
        let addr = addr?;
        // The slot lives inside the cell and is written through this same kind
        // of pointer by a thread holding the cell's stripe, so clone it under
        // that stripe: unguarded, the clone can take a refcount on the node
        // such a writer is dropping (#13016). Released before the caller takes
        // the cell-and-node pair, so this thread waits on one stripe at a time.
        let held = {
            let _structure = crate::value::container_lock::ContainerStructGuard::acquire(addr);
            // SAFETY: `slot` points into the cell's value, which the `Gc` the
            // env entry holds keeps alive for this call; the stripe excludes
            // every other thread's write through the cell (ADR-0068 §7.4).
            unsafe { (*slot).clone() }
        };
        Some((addr, held))
    }

    /// Replace the value the receiver holds (a method that changes the value,
    /// not the container: `Str.subst-mutate`). A binding is written in both
    /// halves of the dual store when the call came through the VM; a detached
    /// container is overwritten.
    // Cost: O(1) for the env entry, O(l) to find the local slot by name, l =
    // local slots of the chunk.
    pub(crate) fn assign(&mut self, interp: &mut Interpreter, new: Value) {
        match self {
            // A receiver with no name has nowhere to be written.
            ReceiverPlace::Var { name: "", .. } => {}
            ReceiverPlace::Var { name, code, .. } => {
                interp.env_mut().insert((*name).to_string(), new.clone());
                if let Some(code) = code {
                    interp.locals_set_by_name(code, name, new);
                }
            }
            ReceiverPlace::Detached(slot) => **slot = new,
        }
    }

    /// Re-seat the receiver, already mutated through its shared node, in both
    /// halves of the dual store (the env entry and the VM's local slot), so a
    /// later locals-to-env sync cannot resurrect a stale snapshot of it. Only a
    /// call that came through the VM (a place built with [`Self::var_in`]) has
    /// a dual store to re-seat; any other receiver has nothing to do.
    // Cost: O(1) for the env entry, O(l) to find the local slot by name, l =
    // local slots of the chunk.
    pub(crate) fn reseat(&self, interp: &mut Interpreter) {
        let ReceiverPlace::Var {
            name,
            value,
            code: Some(code),
            ..
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
        interp.update_local_if_exists(code, name, value);
    }
}
