//! The `async` subsystem of ADR-10779: the interpreter's in-flight state for
//! `gather`/`take`, lazy pulls, `supply`/`react`/`whenever` and the
//! emit buffers between them.

use super::*;

#[derive(Default)]
pub(crate) struct AsyncState {
    pub(crate) gather_items: Vec<Vec<Value>>,
    pub(crate) gather_take_limits: Vec<Option<usize>>,
    pub(crate) supply_emit_buffer: Vec<EmitFrame>,
    /// `whenever` subscription markers registered while a react drive loop is
    /// already running (a `whenever` nested inside another `whenever`'s body).
    /// The loop adopts them on its next round; see
    /// `Interpreter::adopt_newly_registered_subscriptions`.
    pub(crate) pending_react_subscriptions: Vec<Value>,
    /// Sub ids of the callbacks of such nested `whenever`s. A sibling
    /// `whenever` of the react body shares that body's lexicals, so its
    /// callback must re-read them from the live caller env on every value --
    /// hence `call_react_callback` drops the callback's per-instance closure
    /// state. A NESTED `whenever` closes over the *enclosing whenever body's*
    /// frame, which has already exited by the time values arrive, so for it
    /// that per-instance state is the only copy of those lexicals (an
    /// accumulator like `my Buf $in-buf` in HTTP::UserAgent's TestServer) and
    /// dropping it resets them on every value.
    pub(crate) nested_react_callbacks: std::collections::HashSet<u64>,
    /// Emitter `Supplier`s of the `supply` blocks whose code is currently on the
    /// stack, innermost last. `emit` is caught by the innermost *dynamically*
    /// enclosing supply, so a `sub` that is not lexically inside the block still
    /// emits into it when called from within — the parser's `supply` rewrite
    /// (`emit x` -> `$__mutsu_supply_emitter_N.emit(x)`) only reaches `emit`
    /// written directly in the body, and a nested sub's closure never captured
    /// the emitter. Pushed around a `whenever` body invoked as a live-supplier
    /// tap, where the emitter is recovered from the callback's captured env.
    pub(crate) active_supply_emitters: Vec<Value>,
    /// `whenever <Promise>` sources inside a `supply` block, rewritten to a
    /// stand-in supplier and waiting to be armed. A supplier keeps no backlog,
    /// so the promise must not be armed until the consumer has registered the
    /// taps for the rewritten subscription — see
    /// `Interpreter::normalize_promise_whenever_markers`.
    pub(crate) pending_promise_whenever_arms: Vec<(crate::value::SharedPromise, Value)>,
    pub(crate) supply_emit_timed_buffer: Vec<Vec<(Value, crate::thread_compat::Instant)>>,
    /// Active streaming consumers for on-demand `supply { ... }` bodies driven by
    /// `react`. When a stream consumer is registered for an emitter's
    /// `supplier_id`, `emit` delivers the value to the consumer callback
    /// synchronously (instead of buffering into `supply_emit_buffer`), so an
    /// infinite synchronous body (`supply { loop { emit(...) } }`) can be
    /// terminated by the consumer's `done` on emit-to-dead-consumer.
    pub(crate) supply_stream_consumers: Vec<crate::runtime::react_whenever::StreamConsumer>,
    /// Nesting depth of the running `react` drive loop. `> 0` while the event
    /// loop is polling subscriptions and dispatching `whenever`/`LAST`/`QUIT`
    /// callbacks. Used so a `whenever` that taps an on-demand supply from inside
    /// a running react (`whenever $outer { whenever $sod { } }`) routes the
    /// supply's `closing => { ... }` callbacks to the main react thread instead
    /// of firing them on an async body's worker thread (where a write to a
    /// captured react-block lexical would be lost).
    pub(crate) react_active: usize,
    /// Async on-demand supplies tapped by a nested `whenever` while a react drive
    /// loop is running: `(done_signal_promise, closing_callbacks)`. The drive
    /// loop fires each entry's `closing` callbacks on the main thread once the
    /// promise resolves (the emitter signalled `done`), so per-tap
    /// `closing => { ... }` runs on the react thread rather than a worker thread.
    pub(crate) pending_tap_closes: Vec<(crate::value::SharedPromise, Vec<Value>)>,
    /// The waker of the innermost running react/await drive loop on this
    /// thread, so sources wired up mid-loop (a nested `whenever` tapping an
    /// async on-demand supply -> `pending_tap_closes`) can wake the loop when
    /// they become ready instead of waiting out its idle cap.
    pub(crate) current_react_waker: Option<crate::value::waker::ReactWaker>,
    pub(crate) gather_for_loop_resume: Option<crate::value::ForLoopResumeState>,
    /// Transient hand-off from a consumed `ForLoopResumeState` to its loop
    /// executor: the mid-body ip the resumed iteration's first body run
    /// starts at (see `ForLoopResumeState::resume_body_ip`).
    pub(crate) gather_resume_body_ip: Option<usize>,
    /// Set by `take_value` when a lazy pull's take limit is reached inside a
    /// condition-driven loop (`while`/`until`/C-style `loop`): the suspension
    /// is DEFERRED to that loop's next iteration boundary, where re-entering
    /// from the condition on resume is exact. Suspending at the `take` itself
    /// replayed the statements between the take and the iteration end
    /// (`while $n > 1 { take $n; $n div= 2 }` yielded 6,6,6... —
    /// 99problems-31-to-40.t P37).
    pub(crate) gather_suspend_pending: bool,
    /// True while the innermost enclosing loop op is condition-driven
    /// (`while`/`until`/C-style/`repeat`), i.e. a take-limit hit should defer
    /// to its iteration boundary (`gather_suspend_pending`). `for` loops keep
    /// the immediate at-take signal: their positional resume state
    /// (`next_index`) makes the at-take suspension exact for element values,
    /// and roast pins its side-effect timing (S04-statements/gather.t
    /// "gather is lazy"). Saved/restored on loop-op entry/exit.
    pub(crate) lazy_take_boundary_defer: bool,
    /// True while an opcode that `take`s once per element of its own internal
    /// loop (a hyper method call, `@a».take`) is executing in the current
    /// frame. A take-limit hit then parks `gather_suspend_pending` instead of
    /// signalling, and the op suspends after it completes — see
    /// `vm/vm_take_deferring_op.rs` (#9785). Saved/cleared around each lazy
    /// pull so a nested pull's own takes still suspend at the take.
    pub(crate) take_defer_to_op_end: bool,
    /// Call-frame depth (`call_frames.len()`) at entry to the innermost active
    /// lazy-gather pull (`force_lazy_list_vm_n_inner`), `None` outside one.
    /// The pull driver can only snapshot/resume ITS OWN frame (ip, stack,
    /// locals of the gather body's compiled code), so a take-limit hit inside
    /// a NESTED routine call cannot suspend soundly: the signal would unwind
    /// the callee frames and leave the saved ip pointing at the caller's
    /// call op with its arguments already drained (resume then skips the call
    /// or underflows the stack — `gather trip(5)` with `take` inside `trip`'s
    /// `for` loop). `take_value` compares the live depth against this and,
    /// when the take is deeper, parks `gather_suspend_pending` instead of
    /// raising: the pull keeps collecting until a condition-driven loop in
    /// the driver's OWN frame reaches its next iteration boundary, which
    /// happens only after the callee has returned and is a sound suspension
    /// point. `gather_suspend_boundary_reached` compares against this field
    /// again so a loop *inside* the callee leaves the flag alone. The pull may
    /// over-produce but always stops; before the flag was parked, a gather
    /// body whose only takes came from a nested call under an infinite loop
    /// collected forever. Saved/restored around each pull, so nested pulls
    /// compare against their own entry.
    pub(crate) lazy_pull_entry_call_depth: Option<usize>,
    /// Interpreter-path counterpart to `lazy_pull_entry_call_depth`. Some
    /// callbacks are invoked through `call_sub_value`, which records a
    /// `RoutineFrame` but not a VM `call_frames` entry. A take reached through
    /// such a callback is nested just the same and cannot suspend at the take
    /// instruction, because the bounded-pull driver can only resume its own
    /// compiled frame.
    pub(crate) lazy_pull_entry_routine_depth: Option<usize>,
    pub(crate) rw_map_topic_capture: Option<Value>,
    /// Set by the `.map`/`.grep` loops when a `last` in the callback stopped
    /// them, to the loop-handler depth of the loop that caught it
    /// (`loop_handler_depth::loop_handler_depth`). A deferred `.map`/`.grep`
    /// Seq pulled a prefix at a time (`pull_map_grep_prefix`) reads it to tell
    /// "the callback said `last`" from "this chunk of the source ran out",
    /// since the loops swallow the signal; the depth keeps a `last` caught by
    /// a loop nested inside the callback from counting.
    pub(crate) map_grep_last_depth: Option<usize>,
    /// Next routine-invocation id this interpreter will hand out, and one past
    /// the end of the block it was claimed from (see `NEXT_INVOCATION_ID_BLOCK`).
    /// Equal when the block is exhausted, which is the refill condition; both
    /// start at 0, so the first call claims a block (see `take_invocation_id`).
    pub(crate) next_invocation_id: u64,
    pub(crate) invocation_id_block_end: u64,
}

impl AsyncState {
    /// A spawned thread starts with no gather, lazy pull, supply or react in
    /// progress (as `clone_for_thread` did field by field).
    // Cost: O(1).
    pub(crate) fn fork_for_thread(&self) -> Self {
        Self::default()
    }
}
