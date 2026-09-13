//! Lvalue return: an `is rw` routine hands its caller a *container*, and the
//! assignment writes through it (ADR-0059).
//!
//! This is the mechanism that lets a routine expose a storage location reached
//! through its own parameters — `sub g(\c) is rw { return-rw c<a> }`,
//! `method in(\c, *@s) is rw { return-rw c{@s[0]} }` — which the older
//! caller-side re-interpretation of the callee's tail expression structurally
//! cannot do (the caller's frame has no binding for the callee's parameters).
//!
//! The container itself is produced by compiling a `return-rw` operand in
//! container mode (`Compiler::compile_return_rw_arg`); this module is the write
//! half.

use super::*;

impl Interpreter {
    /// Write `value` through a container a routine handed back. Returns `None`
    /// when `container` is an ordinary (non-writable) value, so the caller can
    /// report `X::Assignment::RO` or fall back to the legacy name-based path.
    ///
    /// The container flavours a routine can return, all of which already exist
    /// as `Value` variants:
    /// - `Proxy` — a user-written FETCH/STORE pair; STORE performs the write.
    /// - `ContainerRef` — a shared `Gc<crate::value::ContainerCell>` cell, which is what
    ///   `array_slot_ref` / `hash_slot_ref` promote an existing element to. The
    ///   cell is aliased by identity, so the write is visible through every
    ///   other reference to that element and survives COW clones of the
    ///   enclosing container.
    /// - `HashEntryRef` — the deferred token for a hash key that does not exist
    ///   yet. Writing it walk-creates the intermediate hashes and inserts at the
    ///   terminal key, which is the autovivification a recursive
    ///   path-addressing routine (`Crane::In.in`) depends on.
    pub(crate) fn assign_lvalue_container(
        &mut self,
        container: &Value,
        value: Value,
    ) -> Option<Result<Value, RuntimeError>> {
        match container.view() {
            ValueView::Proxy { .. } => Some(self.assign_proxy_lvalue(container.clone(), value)),
            ValueView::ContainerRef(cell) => {
                // A promoted typed element carries its constraint on the cell
                // (`array_slot_ref` / `hash_slot_ref`), so `tel() = "nope"` on
                // a `my Int @typed` element is rejected here, as `@typed[0] =
                // "nope"` would be.
                if let Err(err) = self.check_container_cell_constraint(&cell, &value) {
                    return Some(Err(err));
                }
                *cell.lock().unwrap() = value.clone();
                Some(Ok(value))
            }
            ValueView::HashEntryRef { .. } => {
                container.hash_entry_write(value.clone());
                Some(Ok(value))
            }
            _ => None,
        }
    }

    /// Whether the legacy `$obj.name($value)` setter convention must NOT be
    /// applied to `$obj.m(args) = value`, because the lvalue-return path owns
    /// this shape and has not been attempted yet.
    ///
    /// An rw-capable method is an lvalue accessor, never a setter: calling it with
    /// the assigned value as its only argument binds that value into the
    /// method's first parameter and produces nonsense (`I.in(%h, "a") = 1`
    /// called `in(1)`, whose `\c` then received `1`). So the setter convention
    /// must not pre-empt it.
    ///
    /// Scoped to a **concrete instance** for a reason of ordering, not of
    /// principle: for an instance the lvalue return runs *later* in the chain
    /// (at the "run the method and inspect its result" site), so blocking the
    /// setter convention is what lets it be reached at all. For a type object
    /// the lvalue return has already run — at the very top of
    /// `assign_method_lvalue_with_values` — and declined, so the remaining
    /// legacy chain is all that is left to try and must not be blocked.
    ///
    /// Also false for the *attribute accessor* shape
    /// (`method x() is rw { $!x }`, `method items { @!items }`): an attribute
    /// tail names its location rather than computing one, is deliberately not
    /// boxed by the container-mode tail compile (`return_rw_container_name`
    /// excludes twigils), and is already handled correctly by the attribute
    /// machinery.
    pub(crate) fn setter_convention_would_preempt_lvalue_return(
        &mut self,
        target: &Value,
        method: &str,
        method_args: &[Value],
    ) -> bool {
        let target = Self::unwrap_lvalue_invocant(target);
        if !matches!(target.view(), ValueView::Instance { .. }) {
            return false;
        }
        self.method_lvalue_returns_container(&target, method, method_args)
    }

    /// Whether `m` is an rw-capable method (`is rw` / `is raw` / `return-rw` —
    /// `method_is_rw_capable`, ADR-0067 slice 2) that *computes* the location it
    /// returns, rather than naming an attribute — the shape the lvalue return
    /// owns.
    fn method_lvalue_returns_container(
        &mut self,
        target: &Value,
        method: &str,
        method_args: &[Value],
    ) -> bool {
        let Some(class_name) = Self::lvalue_invocant_class_name(target) else {
            return false;
        };
        let Some(def) = self.resolve_method(&class_name, method, method_args) else {
            return false;
        };
        Self::method_is_rw_capable(&def) && Self::rw_method_attribute_target(&def.body).is_none()
    }

    /// The method-call half of the lvalue return: `$obj.m(args) = value` where
    /// `m` is `is rw` and returns a container.
    ///
    /// Only invoked for a **type-object invocant** (`Crane::In.in(...) = $v`, a
    /// class-method lvalue), which every instance-oriented path in
    /// `assign_method_lvalue_with_values` rejects outright. An *instance*
    /// invocant reaches the same write through that function's existing
    /// "run the method and inspect its result" site, which already calls the
    /// body exactly once — routing it here as well would call it twice.
    ///
    /// Returns `Ok(None)` when the shape does not apply, leaving the caller's
    /// existing chain untouched.
    pub(crate) fn try_rw_method_container_lvalue(
        &mut self,
        target: &Value,
        method: &str,
        method_args: &[Value],
        value: &Value,
    ) -> Result<Option<Value>, RuntimeError> {
        let target = Self::unwrap_lvalue_invocant(target);
        if !matches!(target.view(), ValueView::Package(_)) {
            return Ok(None);
        }
        if !self.method_lvalue_returns_container(&target, method, method_args) {
            return self.type_object_non_rw_method_lvalue(&target, method, method_args, value);
        }
        let was_lvalue = self.in_lvalue_assignment;
        self.in_lvalue_assignment = true;
        // The pending argument-source names still describe the enclosing
        // `__mutsu_assign_method_lvalue` call, whose first "argument" is the
        // invocant. A sigilless (`\c`) parameter re-reads its argument from the
        // caller's env by that source name, so leaving them in place binds the
        // invocant into the method's first parameter. This call site supplies
        // values, not source names.
        let saved_sources = self.take_pending_call_arg_sources();
        let result = self.call_method_with_values(target.clone(), method, method_args.to_vec());
        self.set_pending_call_arg_sources(saved_sources);
        self.in_lvalue_assignment = was_lvalue;
        // A method that fails when called is not necessarily a failed
        // assignment: the legacy attribute/setter conventions below may still
        // apply (they never call the body). Report the shape as inapplicable and
        // let the existing chain produce the diagnostic.
        let Ok(result) = result else {
            return Ok(None);
        };
        // The write half, shared with the sub form: store through a container or
        // into an `@`/`%` aggregate the tail named (there is no Scalar around an
        // `@`/`%` variable to hand back), and otherwise refuse with rakudo's
        // `Cannot modify an immutable <Type> (<value>)`.
        //
        // The refusal is not a fallback to the legacy setter/attribute chain.
        // That chain re-calls the method with the ASSIGNED VALUE as its only
        // argument — `Crane::In.in(%h, @path) = 9` called `in(9)`, whose `\c`
        // received `9` and whose `*@steps` was empty, so the `.elems == 0`
        // candidate handed `9` straight back and the assignment reported success
        // while writing nowhere. A method this far in is rw-capable and computes
        // its location (`method_lvalue_returns_container`), so a plain value
        // coming back IS raku's refusal, which is what
        // `Crane::Set`'s `CATCH { when X::Assignment::RO }` maps to
        // `X::Crane::OpSet::RO`.
        self.assign_through_rw_result(result, value.clone())
            .map(Some)
    }

    /// `Class.m($arg) = $v` where `m` is a declared method that is **not**
    /// rw-capable: raku dies (`Cannot modify an immutable Int (42)`) after
    /// calling `m` with its real arguments.
    ///
    /// mutsu used to fall through to the legacy `$obj.name($value)` setter
    /// convention here, which re-called `m` with the *assigned value* as its
    /// only argument (or, for a sigilless parameter, with the invocant) and
    /// then reported the assignment as done — so the write silently vanished
    /// and the program continued. The instance twin never reached that: it hits
    /// the "cannot assign through .m on non-instance" / "method 'm' is not rw"
    /// refusals first.
    ///
    /// The gate is the same declaration oracle the instance path uses
    /// (`method_is_rw_capable`, ADR-0067 slice 2), so only a method the class
    /// actually declares is affected. Anything the class does not declare — a
    /// builtin, an `AT-KEY`-shaped element accessor, a name resolved elsewhere
    /// — answers `Ok(None)` and the legacy chain is untouched, and so is the
    /// attribute-accessor shape (`method x() { $!x }`), which names its
    /// location rather than computing one and is handled by the attribute
    /// machinery.
    ///
    /// Argument-less (`Class.m() = $v`) is left alone too: the legacy chain
    /// requires a non-empty argument list, so there is nothing to preempt.
    fn type_object_non_rw_method_lvalue(
        &mut self,
        target: &Value,
        method: &str,
        method_args: &[Value],
        value: &Value,
    ) -> Result<Option<Value>, RuntimeError> {
        if method_args.is_empty() || matches!(method, "AT-KEY" | "AT-POS") {
            return Ok(None);
        }
        let Some(class_name) = Self::lvalue_invocant_class_name(target) else {
            return Ok(None);
        };
        let Some(def) = self.resolve_method(&class_name, method, method_args) else {
            return Ok(None);
        };
        // rw-capable methods never reach here (the caller took the other
        // branch); an attribute accessor is excluded for the reason above.
        if Self::rw_method_attribute_target(&def.body).is_some() {
            return Ok(None);
        }
        let was_lvalue = self.in_lvalue_assignment;
        self.in_lvalue_assignment = true;
        // As in `try_rw_method_container_lvalue`: the pending argument-source
        // names still describe the enclosing `__mutsu_assign_method_lvalue`
        // call, whose first "argument" is the invocant, so a sigilless (`\x`)
        // parameter would re-read the invocant by that name. This call site
        // supplies values.
        let saved_sources = self.take_pending_call_arg_sources();
        let result = self.call_method_with_values(target.clone(), method, method_args.to_vec());
        self.set_pending_call_arg_sources(saved_sources);
        self.in_lvalue_assignment = was_lvalue;
        // A method that throws when called is not a failed *assignment*: leave
        // the existing chain to produce the diagnostic, exactly as the rw path
        // does.
        let Ok(result) = result else {
            return Ok(None);
        };
        // Writes through if the body happened to hand back a container anyway;
        // otherwise this is raku's `Cannot modify an immutable <Type> (<value>)`.
        self.assign_through_rw_result(result, value.clone())
            .map(Some)
    }

    /// The assignment call site wraps the invocant in a `VarRef` (its source
    /// name rides along for the attribute writeback paths). Dispatch on the
    /// value itself: a `VarRef` invocant makes the method dispatcher treat the
    /// call as a writable-variable receiver and re-derive its argument list,
    /// which drops the real first positional.
    fn unwrap_lvalue_invocant(target: &Value) -> Value {
        match target.as_varref() {
            Some((_, inner, _)) => inner.clone(),
            None => target.clone(),
        }
    }

    /// The class name to resolve a method against for an lvalue assignment.
    /// Covers both a concrete instance and a type object used as an invocant
    /// (`Crane::In.in(...) = $v` — a class-method lvalue), which the
    /// instance-only paths reject outright.
    fn lvalue_invocant_class_name(target: &Value) -> Option<String> {
        match target.view() {
            ValueView::Instance { class_name, .. } => Some(class_name.resolve()),
            ValueView::Package(name) => Some(name.resolve()),
            _ => None,
        }
    }
}
