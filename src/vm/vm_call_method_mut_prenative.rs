use super::*;

impl Interpreter {
    /// Method names either `CallMethod` or `CallMethodMut` gives a dedicated
    /// branch between its scoped-env flatten and its general native probe that
    /// can apply to a plain scalar receiver: the undeclared-type `.new` check on
    /// a `Str`, `subst-mutate`, the `hyper`/`race` configuration form, the
    /// xxx-KEY / BIND-POS / `add`/`remove` mutators (their `Nil`/type-object
    /// arms), the `push`-family autovivification, `VAR` (which
    /// `exec_call_method_op_impl` hard-codes into its `skip_native` seed), and
    /// the Lock/Match/WHO arms (receiver-typed, listed for completeness). A call
    /// to any of them keeps the full dispatch path so that branch still sees it.
    ///
    /// The list is shared by both opcodes rather than split per opcode: it only
    /// ever makes the gate MORE conservative, and a name that has a dedicated
    /// branch on one path is not worth gating on the other.
    fn dispatch_branches_before_native_probe(method: &str) -> bool {
        matches!(
            method,
            "new"
                | "VAR"
                | "WHO"
                | "protect"
                | "protect-or-queue-on-recursion"
                | "with-lock-hidden-from-recursion-check"
                | "make"
                | "subst-mutate"
                | "hyper"
                | "race"
                | "AT-KEY"
                | "ASSIGN-KEY"
                | "DELETE-KEY"
                | "BIND-KEY"
                | "BIND-POS"
                | "add"
                | "remove"
                | "push"
                | "unshift"
                | "append"
                | "prepend"
        )
    }

    /// `CallMethodMut` dispatches that provably never look at the lexical
    /// environment, answered BEFORE the opcode's `flatten_scoped_env` guard.
    ///
    /// The guard exists because full method dispatch may capture the env into
    /// a closure or run an interpreter fallback that iterates it, and a scoped
    /// overlay env would hand such a consumer a truncated view. Two dispatch
    /// shapes cannot reach any such consumer, and they are exactly the two a
    /// vendored `Test.rakumod` assertion makes (`$desc.Str`, `$output.say`):
    ///
    /// 1. **A pure native method on an immutable scalar receiver** (`Str`,
    ///    `Int`, `Num`, `Bool`) -- lifted into
    ///    [`Self::try_env_pure_scalar_native_dispatch`], which the plain
    ///    `CallMethod` opcode shares. Everything the general path would have
    ///    decided for such a receiver is decided identically there -- see that
    ///    function for the exclusions and for why no dedicated pre-probe branch
    ///    of either opcode is stolen.
    /// 2. **Text output to a native `IO::Handle`** (`print`/`put`/`say`/
    ///    `printf`/`print-nl` on an exact `IO::Handle` instance, no augmented
    ///    override). `try_native_io_handle_output` writes handle-table state
    ///    and renders its arguments; it neither captures nor iterates the
    ///    caller's env. A user subclass has a different class name and keeps
    ///    the full path (its `WRITE` override is what `try_user_io_handle_method`
    ///    routes to).
    ///
    /// Why this matters: the guard's flatten on a scoped env is a whole-scope
    /// map clone, and it also turns the enclosing routine's return merge and
    /// env drop from overlay-sized into scope-sized. Measured on the vendored
    /// `Test` assertion loop the guard and its consequences were ~7.5% of the
    /// per-assertion budget, with these two calls the only dispatches that
    /// paid it (vendor-real-test-module-flip (#7554), and the record of the
    /// wholesale relocation that did NOT pay in
    /// method-dispatch-flattens-the-env-on-every-call (#7563)).
    ///
    /// Returns `None` to continue with the general path unchanged.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn try_env_pure_mut_dispatch(
        &mut self,
        target: &Value,
        method: &str,
        method_sym: crate::symbol::Symbol,
        args: &[Value],
        modifier: Option<&str>,
        quoted: bool,
    ) -> Option<Result<Value, RuntimeError>> {
        if modifier.is_some() || quoted {
            return None;
        }
        if let Some(result) = self.try_env_pure_scalar_native_dispatch(
            "callmethodmut",
            target,
            method,
            method_sym,
            args,
            modifier,
            quoted,
        ) {
            return Some(result);
        }
        if let Some(result) =
            self.try_env_pure_type_object_defined(target, method, method_sym, args)
        {
            return Some(result);
        }
        if let ValueView::Instance { class_name, .. } = target.view()
            && class_name == "IO::Handle"
            && matches!(method, "print" | "put" | "say" | "printf" | "print-nl")
            && !self.has_user_method_sym("IO::Handle", method_sym)
        {
            let result = self.try_native_io_handle_output(target, method, args)?;
            // Same bookkeeping as the general path's user-dispatch completion
            // this used to reach: the rendering may run user `gist` code, so
            // the dispatch is not assumed env-pure.
            self.method_dispatch_pure = false;
            crate::vm::vm_stats::record_dispatch_entry_outcome("callmethodmut", "user");
            self.shadow_check_native_row_candidate(target, method, method_sym, args.len(), false);
            return Some(result);
        }
        None
    }

    /// Case 3 of [`Self::try_env_pure_mut_dispatch`]: `.defined` on a type
    /// object -- an uninitialized typed variable or attribute
    /// (`has Str $.text; ... !$!text.defined`), the commonest shape a
    /// definedness test takes in OO code (#9494, Text::CSV's
    /// `CSV::Field.undefined`, once per parsed field).
    ///
    /// A type object is an immutable value, so the native answer cannot write
    /// anything back. The general path decides this receiver exactly so: it
    /// sets `skip_native` when the class (or, for a type object, an applicable
    /// candidate) declares its own `defined`, consults the lever-A augment
    /// gate, and otherwise lets `try_native_method` answer -- every other
    /// branch between its flatten guard and that probe is keyed on a method
    /// name other than `defined` or on a receiver kind other than `Package`.
    /// The `Nil` type object is left alone: the pre-dispatch Nil arm owns it.
    // Cost: O(1) on the memoized user-method probes.
    fn try_env_pure_type_object_defined(
        &mut self,
        target: &Value,
        method: &str,
        method_sym: crate::symbol::Symbol,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let ValueView::Package(class_sym) = target.view() else {
            return None;
        };
        if method != "defined"
            || !args.is_empty()
            || target.is_nil()
            || class_sym == "Nil"
            || self.grammar_has_user_method_memo(class_sym, method_sym)
            || self.package_has_applicable_user_method(target, method, args)
            || self.native_lever_a_user_override_sym(target, method_sym)
        {
            return None;
        }
        let native_result = self.try_native_method(target, method_sym, args)?;
        self.method_dispatch_pure = true;
        crate::vm::vm_stats::record_dispatch_entry_outcome("callmethodmut", "native");
        Some(native_result)
    }

    /// Case 1 of [`Self::try_env_pure_mut_dispatch`], shared with the plain
    /// `CallMethod` opcode: **a pure native method on an immutable scalar
    /// receiver** (`Str`, `Int`, `Num`, `Bool`), answered BEFORE either
    /// opcode's `flatten_scoped_env` guard.
    ///
    /// `try_native_method` is Rust over the value; an immutable receiver has no
    /// writeback, so none of the by-name or by-identity env scans the container
    /// mutators perform can be reached, and nothing between the guard and the
    /// native probe captures or iterates the env for such a receiver. Every
    /// decision the general path would have made for it is made identically
    /// here: the same `quoted`/modifier exclusions, the same junction-argument
    /// autothreading deferral, the same `native_lever_a_user_override` gate (an
    /// augmented `Str.uc` still wins), and the methods that own a dedicated
    /// branch on either path are left to it
    /// ([`Self::dispatch_branches_before_native_probe`]). Both paths' remaining
    /// pre-probe branches are receiver-typed on values this arm never accepts
    /// (`Junction`, `LazyList`, `Failure`, `Package`, `Instance`, `Array`,
    /// `Regex`, `Proxy`), so none of them is stolen.
    ///
    /// `entry` is the dispatch-entry label the caller reports under
    /// (`"callmethod"` / `"callmethodmut"`).
    ///
    /// Returns `None` to continue with the general path unchanged.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn try_env_pure_scalar_native_dispatch(
        &mut self,
        entry: &str,
        target: &Value,
        method: &str,
        method_sym: crate::symbol::Symbol,
        args: &[Value],
        modifier: Option<&str>,
        quoted: bool,
    ) -> Option<Result<Value, RuntimeError>> {
        if modifier.is_some() || quoted {
            return None;
        }
        if !matches!(
            target.view(),
            ValueView::Str(_) | ValueView::Int(_) | ValueView::Num(_) | ValueView::Bool(_)
        ) {
            return None;
        }
        if Self::dispatch_branches_before_native_probe(method)
            || args
                .iter()
                .any(|a| matches!(a.view(), ValueView::Junction { .. }))
            || self.native_lever_a_user_override_sym(target, method_sym)
        {
            return None;
        }
        let native_result = self.try_native_method(target, method_sym, args)?;
        // Same bookkeeping as the general path's native completion: a
        // value-returning native on an immutable receiver is env-pure.
        self.method_dispatch_pure = true;
        crate::vm::vm_stats::record_dispatch_entry_outcome(entry, "native");
        self.shadow_check_native_row_candidate(target, method, method_sym, args.len(), true);
        Some(native_result)
    }

    /// `.elems` / `.end` on an `Array` value (the stack top), answered by the
    /// native method the general `CallMethodMut` path would reach for it.
    ///
    /// Every branch between that opcode's top and its native probe is keyed on
    /// an argument, a modifier, a method name other than these two, or a
    /// receiver kind an `Array` is not -- except the lazy-list guard, which is
    /// why a lazy array (`my @a = 1..*`, whose count is not known) is left to
    /// it. A user `^find_method` or an augmented `Array.elems` (lever A) also
    /// keeps the full path. Neither method writes to its receiver, so the
    /// dispatch is env-pure.
    // Cost: O(1) to decide; the native method's own cost.
    pub(super) fn try_array_count_lane(
        &mut self,
        method: &str,
        method_sym: crate::symbol::Symbol,
    ) -> Option<Result<Value, RuntimeError>> {
        if !matches!(method, "elems" | "end")
            || crate::runtime::find_method_intercept::any_user_find_method()
        {
            return None;
        }
        let target = self.stack.last()?;
        if !matches!(target.view(), ValueView::Array(_, kind) if !kind.is_lazy()) {
            return None;
        }
        let target = target.clone();
        if self.native_lever_a_user_override_sym(&target, method_sym) {
            return None;
        }
        let native_result = self.try_native_method(&target, method_sym, &[])?;
        self.method_dispatch_pure = true;
        crate::vm::vm_stats::record_dispatch_entry_outcome("callmethodmut", "native");
        Some(native_result)
    }
}
