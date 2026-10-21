use super::*;

/// How many elements a plain `gather` returned from a `.map`/`.grep` callback
/// is pulled before it is left lazy (see
/// [`Interpreter::reify_finite_pipe_value`]). Matches the historical strict-force
/// prefix cap; a gather that has not finished by then is treated as possibly
/// infinite and keeps its suspended coroutine so later pulls resume it.
pub(super) const MAP_RESULT_GATHER_FORCE_CAP: usize = 100_000;

impl Interpreter {
    /// Reify a `.map`/`.grep` callback result that would otherwise reach the
    /// result list as an unforced lazy element.
    ///
    /// Those elements land in a container whose *static* readers
    /// (`flat_val`/`value_to_list`, gist/raku rendering, hash assignment)
    /// cannot run the VM to force them, so they read the still-empty cache and
    /// yield `()`. Two shapes are reified into a single `Seq` element (never
    /// flattened — raku `(1,).map({(10,20)})` == `((10 20),)`):
    ///
    /// - a lazy `.map`/`.grep` pipe, not `lazy`-marked, whose source chain bottoms out in a
    ///   provably-finite source (a `gather`, or a finite Array/Seq/Range). An
    ///   infinite pipe (`(1,).map({1..Inf})`) bottoms out `false` and stays
    ///   lazy, so this can never turn an infinite pipe into a hang;
    /// - a plain (non-`lazy`-marked) `gather` Seq (`(1,).map({ gather take 5 })`,
    ///   #9584). Whether a gather ends is undecidable, so it gets a *bounded*
    ///   pull of [`MAP_RESULT_GATHER_FORCE_CAP`] elements: when the body runs to
    ///   completion within it the element becomes a reified `Seq`; otherwise the
    ///   element stays the same `LazyList`, whose pulled prefix is cached and
    ///   whose suspended coroutine resumes on the next pull — so
    ///   `(1,).map({ gather loop { take 1 } }).head.head` still answers `1`.
    ///
    /// A nested `.map`/`.grep` `Seq` stays deferred here. The eventual reader
    /// reifies it when it actually needs the nested elements.
    ///
    /// Cost: O(k) for a finite pipe or gather producing k elements
    /// (k ≤ MAP_RESULT_GATHER_FORCE_CAP for a gather); O(1) for other values.
    pub(crate) fn reify_finite_pipe_value(&mut self, val: Value) -> Result<Value, RuntimeError> {
        // A lazy-tail Slip flattens into the result: its ending lazy items
        // are reified first (#13117).
        if val.is_lazy_tail_slip() {
            return self.reify_lazy_slip_tail(val);
        }
        if let ValueView::LazyList(ll) = val.view() {
            // A `lazy`-marked pipe (`lazy gather take 5`, `(1..3).lazy.map(..)`)
            // is `.is-lazy` in Rakudo whatever its source, so it stays lazy.
            if ll.lazy_pipe.is_some() && !ll.is_lazy_marked() && ll.pipe_bottoms_out_finite() {
                let items = self.force_lazy_list_vm(&ll)?;
                return Ok(Value::seq(items));
            }
            if ll.coroutine.is_some()
                && ll.sequence_spec.is_none()
                && ll.scan_spec.is_none()
                && ll.lazy_pipe.is_none()
                && !ll.is_lazy_marked()
            {
                let items = self.force_lazy_list_vm_n(&ll, MAP_RESULT_GATHER_FORCE_CAP)?;
                let finished = ll
                    .coroutine
                    .as_ref()
                    .is_some_and(|coro| coro.lock().unwrap().finished);
                if finished {
                    return Ok(Value::seq(items));
                }
            }
        }
        Ok(val)
    }
}
