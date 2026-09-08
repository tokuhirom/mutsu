use super::*;

impl Interpreter {
    /// Method names `exec_call_method_mut_op_impl` gives a dedicated branch
    /// between its scoped-env flatten and its general native probe that can
    /// apply to a plain scalar receiver: the undeclared-type `.new` check on a
    /// `Str`, `subst-mutate`, the `hyper`/`race` configuration form, the
    /// xxx-KEY / BIND-POS / `add`/`remove` mutators (their `Nil`/type-object
    /// arms), the `push`-family autovivification, and the Lock/Match/WHO
    /// arms (receiver-typed, listed for completeness). A call to any of them
    /// keeps the full dispatch path so that branch still sees it.
    fn mut_dispatch_branches_before_native_probe(method: &str) -> bool {
        matches!(
            method,
            "new"
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
    ///    `Int`, `Num`, `Bool`). `try_native_method` is Rust over the value;
    ///    an immutable receiver has no writeback, so none of the by-name or
    ///    by-identity env scans the container mutators perform can be
    ///    reached. Everything the general path would have decided for such a
    ///    receiver is decided identically here: the same `quoted`/modifier
    ///    exclusions, the same junction-argument autothreading deferral, the
    ///    same `native_lever_a_user_override` gate (an augmented `Str.uc`
    ///    still wins), and the methods that own a dedicated branch on the
    ///    general path are left to it (`mut_dispatch_branches_before_native_probe`).
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
        if matches!(
            target.view(),
            ValueView::Str(_) | ValueView::Int(_) | ValueView::Num(_) | ValueView::Bool(_)
        ) {
            if Self::mut_dispatch_branches_before_native_probe(method)
                || args
                    .iter()
                    .any(|a| matches!(a.view(), ValueView::Junction { .. }))
                || self.native_lever_a_user_override(target, method)
            {
                return None;
            }
            let native_result = self.try_native_method(target, method_sym, args)?;
            // Same bookkeeping as the general path's native completion: a
            // value-returning native on an immutable receiver is env-pure.
            self.method_dispatch_pure = true;
            crate::vm::vm_stats::record_dispatch_entry_outcome("callmethodmut", "native");
            self.shadow_check_native_row_candidate(target, method, method_sym, args.len(), true);
            return Some(native_result);
        }
        if let ValueView::Instance { class_name, .. } = target.view()
            && class_name == "IO::Handle"
            && matches!(method, "print" | "put" | "say" | "printf" | "print-nl")
            && !self.has_user_method("IO::Handle", method)
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
}
