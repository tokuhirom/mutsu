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
    /// - `ContainerRef` — a shared `Gc<Mutex<Value>>` cell, which is what
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
    /// An `is rw` method is an lvalue accessor, never a setter: calling it with
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
    /// (`method x() is rw { $!x }`, `method items { @!items }`): a bare variable
    /// tail names its location rather than computing one, is not yet compiled to
    /// a container return (ADR-0059 Slice 2), and is already handled correctly by
    /// the attribute machinery.
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

    /// Whether `m` is an `is rw` method that *computes* the location it returns,
    /// rather than naming an attribute — the shape the lvalue return owns.
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
        def.is_rw && Self::rw_method_attribute_target(&def.body).is_none()
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
            return Ok(None);
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
        match self.assign_lvalue_container(&result, value.clone()) {
            Some(assigned) => assigned.map(Some),
            None => Ok(None),
        }
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
