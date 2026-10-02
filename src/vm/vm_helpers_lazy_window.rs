use super::*;

impl Interpreter {
    /// Force a lazy list to produce at least `needed` elements and answer
    /// elements `from..needed` (clamped to what it produces).
    ///
    /// A gather uses coroutine-style suspend/resume: the body pauses at each
    /// `take` once enough elements are available, and can be resumed later.
    /// Side effects (e.g. `$count++`) are correctly scoped because we pause
    /// mid-execution rather than re-running from scratch.
    ///
    /// A consumer that walks the list one element at a time (a `for` loop,
    /// a pipe stage pulling its source) asks for just the new element, so
    /// each pull copies O(1) elements rather than the whole reified prefix
    /// (#10780).
    // Cost: O(needed - from) for the copy, plus whatever producing the
    // missing elements costs.
    pub(crate) fn force_lazy_list_vm_window(
        &mut self,
        list: &LazyList,
        from: usize,
        needed: usize,
    ) -> Result<Vec<Value>, RuntimeError> {
        // GC safepoint (§9.2a `lazy_force`): the bounded pull/resume boundary.
        crate::vm::vm_poll::poll(crate::gc::SafepointKind::LazyForce, 0);
        let caller_code = self.current_code;
        // The body runs under its OWN readonly context, not the consumer
        // frame's (see take_readonly_state).
        let saved_readonly = self.take_readonly_state();
        // See `force_lazy_list_vm`: the body's `samewith` is lexical.
        let pushed_samewith = self.push_captured_samewith_context(&list.env);
        let saved_unit = list
            .env
            .get("__mutsu_gather_unit")
            .and_then(|value| match value.view() {
                ValueView::Str(unit) => Some(std::mem::replace(
                    &mut self.current_unit,
                    crate::symbol::Symbol::intern(unit.as_str()),
                )),
                _ => None,
            });
        // See `force_lazy_list_vm`: restore the package the gather was
        // WRITTEN in for the duration of this (possibly resumed) pull.
        let saved_package = self.enter_gather_package(&list.env);
        // A lazy gather body runs in its own captured env, not the forcing
        // frame's, so it blocks the inline CATCH chain (ADR-0072).
        // A method call, as in `force_lazy_list_vm` (#10746).
        let r = self.with_catch_marker(|this| {
            this.in_method_call(|this| this.force_lazy_list_vm_window_inner(list, from, needed))
        });
        if let Some(pkg) = saved_package {
            self.set_current_package(pkg);
        }
        if let Some(unit) = saved_unit {
            self.current_unit = unit;
        }
        self.pop_captured_samewith_context(pushed_samewith);
        self.restore_readonly_state(saved_readonly);
        self.reconcile_caller_after_lazy_force(caller_code);
        // See force_lazy_list_vm: array-context elements store Any, not Nil,
        // and are itemized like any other element store.
        if list.in_array_context() {
            return match r {
                Ok(items) => Ok(Self::itemize_lazy_array_elements(
                    self.decay_nil_vec_elements(items),
                )),
                err => err,
            };
        }
        r
    }
}
