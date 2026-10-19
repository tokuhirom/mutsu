//! The lazy step: what a method call does to a lazy receiver (ADR-13891).
//!
//! The twin of the `Seq` consumption step (`reify_or_consume_seq_target`): it
//! runs before the receiver's rows and decides, for one call on a `LazyList`,
//! one of three things.
//!
//! 1. The method has a *pull* on the type the receiver reports
//!    (`method_table::lazy_pull`): it answers from what it pulls (`AT-POS`,
//!    `cache`, the placeholders of `gist`/`Str`/`raku`/`join`).
//! 2. The method needs every element (`method_table::lazy_pull::reifies`): the receiver is
//!    reified and the call re-dispatched on a settled `Seq` of its elements,
//!    whose rows answer it.
//! 3. Neither: the call goes on to the later steps unchanged.

use super::*;

impl Interpreter {
    /// Run the lazy step for `method` on the lazy receiver `target` (whose
    /// list is `list`). `None` when the call goes on unchanged (step 3).
    // Cost: O(p) to find the pull, p = registered pulls; then the pull's own
    // cost, or O(e) to reify the receiver, e = its elements, plus the
    // re-dispatched call.
    pub(super) fn lazy_receiver_step(
        &mut self,
        target: &Value,
        list: &crate::gc::Gc<crate::value::LazyList>,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if let Some(pull) = crate::builtins::method_table::lazy_pull::pull_of(target, method)
            && let Some(answer) = pull(self, target, list, args)
        {
            return Some(answer);
        }
        if !crate::builtins::method_table::lazy_pull::reifies(method)
            // A `.map`/`.grep` on a lazy map/grep pipeline, an infinite
            // sequence/closure spec, OR a gather coroutine appends another lazy
            // stage (`dispatch_map_method`/`dispatch_grep` via
            // `is_lazy_pipe_source`); a laziness-preserving coercion returns the
            // list unchanged. Neither reifies the (possibly infinite) sequence (L2b).
            || (matches!(method, "map" | "grep") && list.map_grep_appends_stage())
            || ((list.lazy_pipe.is_some() || list.is_infinite_spec())
                && Interpreter::lazy_pipe_preserving_coercion(method))
        {
            return None;
        }
        Some(self.reify_lazy_receiver(list, method, args))
    }

    /// Step 2: reify `list` and re-dispatch the call on a `Seq` of its elements.
    // Cost: O(e), e = elements reified, plus the re-dispatched call.
    fn reify_lazy_receiver(
        &mut self,
        list: &crate::gc::Gc<crate::value::LazyList>,
        method: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        // A single-pass `.lazy` Seq throws on a second consuming touch
        // (`claim_lazy_seq_touch`'s doc comment, #9789); the table also names
        // non-consuming methods (`gist`, `elems`, ...) that must keep reading
        // it repeatably, so gate on the same `seq_method_consumes` table a
        // real `Value::Seq` uses.
        if crate::value::seq_method_consumes(method) {
            crate::value::claim_lazy_seq_touch(list)?;
        }
        let saved_env = self.env.clone();
        let items = self.force_lazy_list_bridge(list)?;
        // A successful `.sink` consumes the original gather Seq as well as the
        // temporary Seq used for re-dispatch. Mark only after the bridge
        // returns: a gather body that throws was not sunk.
        if method == "sink" && list.is_from_gather() {
            crate::value::lazylist_consume(list);
        }
        if !matches!(method, "elems" | "hyper" | "race") {
            self.env = saved_env;
        }
        self.call_method_with_values(Value::seq(items), method, args.to_vec())
    }
}
