//! Sinking a lazy `map`/`grep` pipe inside a lazily pulled `gather`.
//!
//! `(1..*).map: { take $_ }` as the (sunk) statement of a `gather` that is
//! pulled a bounded number of elements at a time must suspend after each
//! `take`, as Rakudo does: it sinks the Seq element by element. Forcing the
//! pipe whole (`force_lazy_list_vm`) instead throws `X::Cannot::Lazy` for an
//! infinite source, because a `take` inside the callback runs deeper than the
//! pull driver and can only park `gather_suspend_pending`.
//!
//! The pipe object already carries its own state (source position, cache), so
//! the sink loop pulls one element at a time and, when a pending suspension
//! becomes reachable again in the driver's frame, leaves the pipe on the stack
//! and parks the ip on the `SinkPop`: a resume re-enters this loop and the next
//! pull continues the same pipe.

use super::*;

impl Interpreter {
    /// Sink `list` (a lazy map/grep pipe already popped off the stack as `val`)
    /// element by element when that is sound, i.e. the op runs in the frame of
    /// a bounded lazy-gather pull outside any loop op of the gather body.
    ///
    /// `Ok(true)`: the pipe ended and is sunk. `Ok(false)`: not applicable, the
    /// caller forces the pipe as before. `Err`: the take-limit signal, with
    /// `val` pushed back for the resume.
    // Cost: O(k) pulled elements, k = elements until the next suspension or the
    // end of the pipe, plus the stage callbacks.
    pub(super) fn sink_lazy_pipe_in_gather(
        &mut self,
        list: &LazyList,
        val: &Value,
        site: (usize, usize),
    ) -> Result<bool, RuntimeError> {
        if list.lazy_pipe.is_none()
            || !matches!(self.async_state.gather_take_limits.last(), Some(Some(_)))
            || self.async_state.lazy_take_boundary_defer
            || self.async_state.take_defer_to_op_end
            || self.async_state.lazy_pull_entry_call_depth != Some(self.call_frames.len())
            || self.async_state.lazy_pull_entry_routine_depth
                != Some(self.lazy_pull_routine_depth())
            || self.async_state.lazy_pull_entry_loop_depth
                != Some(self.topic_state.loop_local_vars.len())
        {
            return Ok(false);
        }
        match self.async_state.gather_for_loop_resume {
            None => {}
            Some(crate::value::ForLoopResumeState::SunkPipe { .. }) => {
                self.async_state.gather_for_loop_resume = None;
            }
            // A loop op's resume state is waiting for its own op.
            Some(_) => return Ok(false),
        }
        loop {
            let pipe_done = |list: &LazyList| {
                list.lazy_pipe
                    .as_ref()
                    .is_none_or(|p| p.lock().unwrap_or_else(|e| e.into_inner()).done)
            };
            if pipe_done(list) {
                return Ok(true);
            }
            let want = list
                .cache
                .lock()
                .unwrap_or_else(|e| e.into_inner())
                .as_ref()
                .map_or(0, Vec::len)
                + 1;
            self.fill_lazy_pipe(list, want)?;
            // The sunk values are discarded; keep the cache from growing with
            // every element of an endless pipe.
            *list.cache.lock().unwrap_or_else(|e| e.into_inner()) = Some(Vec::new());
            if self.gather_suspend_boundary_reached() {
                self.async_state.gather_suspend_pending = false;
                self.stack.push(val.clone());
                self.async_state.gather_for_loop_resume =
                    Some(crate::value::ForLoopResumeState::SunkPipe { site });
                return Err(RuntimeError::new(Self::LAZY_GATHER_TAKE_LIMIT_SIGNAL));
            }
        }
    }
}
