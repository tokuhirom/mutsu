//! The consumer behind a `.tap` on a channel-backed live Supply
//! (`IO::Socket::Async` reads, `Supply.interval`, `signal`, `Proc::Async`
//! streams, the `ThreadPoolScheduler` drain pump).
//!
//! The loop is event-driven (issue #12892). It holds no thread while its tap is
//! idle: a [`DrainHook`](super::supply_channel::DrainHook) on the channel
//! schedules one pool task when an event arrives, and that task drains
//! whatever is queued, then returns its worker.
//! Rakudo works the same way: it dispatches each emitted value as a scheduler
//! task, and no thread waits on behalf of an idle tap. The previous design kept
//! a pool worker blocked on the channel for the whole tap lifetime, waking every
//! 250 ms to re-check its close flag, so N tapped connections cost N threads.
//!
//! Two flags keep the drains of one tap serial and lose no wake-up:
//! `scheduled` is held by the one drain that may run (so the tap callback
//! still runs one body at a time, in order), and `dirty` records a kick that
//! arrived while a drain was running. A drain clears `dirty` before it
//! reads the channel, releases `scheduled` once the channel is empty, and
//! re-claims `scheduled` if `dirty` was set again in between.
//!
//! wasm32 keeps the blocking loop: its pool is the cooperative scheduler, where
//! a blocked task yields and there is no thread to save.
use crate::runtime::*;

use super::state::{SupplyEvent, take_complete_lines_from_buffer};
use super::supply_channel::SupplyReceiver;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};

/// One dispatch: the value to pass to the plain callback (unused when the
/// second field is set), an optional `(callback, args)` to call instead (the
/// tap's `done =>`/`quit =>` handler), and whether that handler is a
/// done-group marker/`__SupplyDoneChain` that needs `invoke_done_callback`
/// rather than a plain call.
type DispatchUnit = (Value, Option<(Value, Vec<Value>)>, bool);

/// Whether the act loop keeps going after an event.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Flow {
    Continue,
    Finish,
}

/// The state of one tap's act loop: what to call, and what it has carried over
/// between events.
///
/// `done_cb` / `quit_cb` are the tap's `done =>` / `quit =>` handlers: a
/// channel-backed source signals the end of the stream with a
/// `SupplyEvent::Done`/`Quit`, and dropping out of the loop without running
/// them left an `IO::Socket::Async` reader waiting forever for the `done` that
/// a closed peer had already sent (roast S32-io/IO-Socket-Async.t "Echo
/// server").
///
/// `close` is the Tap-teardown handle (`register_act_loop_close` id plus the
/// tap's close flag): `Tap.close` sets the flag and kicks the loop, whose next
/// drain exits. `None` (the scheduled-pump drain, whose sender is dropped on
/// close) ends only on a disconnect. A flag-driven exit is a *close*, not a
/// `done` — it must not run the done chain (raku does not fire LAST phasers on
/// `.close`; CLOSE phasers are fired separately by `native_tap`).
///
/// `is_lines`/`line_chomp` mirror the `.lines`-derived Supply's own
/// attributes: when set, a received chunk is appended to a carry-over buffer
/// and split into complete lines (`take_complete_lines_from_buffer`, the same
/// splitter the react/whenever drive loop uses) instead of being forwarded to
/// `cb` verbatim — a TCP read boundary can land mid-line, so a per-chunk split
/// with no carry-over would emit truncated lines. Any partial trailing line is
/// flushed once as its own line when the source signals `Done`/`Quit`.
///
/// `head_limit` mirrors a `.head(N)`-derived Supply's own attribute (see the
/// "head" arm in `native_supply_dispatch.rs`): once `N` plain-value units have
/// been dispatched, the loop stops accepting more even though the channel
/// itself never signals `Done` (an infinite source like `Supply.interval` has
/// no natural end) — it fires `done_cb` itself and finishes, same as a real
/// upstream `Done`.
///
/// `producer_supplier_id` is set when `cb` is *producer* code — a
/// `supply { }` block's `whenever` body, driven here because its source is a
/// live channel — so its failure quits the enclosing supply (issue #8185).
pub(in crate::runtime) struct ActLoop {
    pub(in crate::runtime) interp: Interpreter,
    pub(in crate::runtime) cb: Value,
    pub(in crate::runtime) delay_seconds: f64,
    pub(in crate::runtime) done_cb: Option<Value>,
    pub(in crate::runtime) quit_cb: Option<Value>,
    pub(in crate::runtime) close: Option<(u64, Arc<AtomicBool>)>,
    pub(in crate::runtime) is_lines: bool,
    pub(in crate::runtime) line_chomp: bool,
    pub(in crate::runtime) head_limit: Option<usize>,
    pub(in crate::runtime) producer_supplier_id: Option<u64>,
    pub(in crate::runtime) line_buffer: String,
}

/// Completion of an act loop, for a spawner that must wait until the tap has
/// seen its whole stream (the `Proc::Async` exit path).
pub(in crate::runtime) struct ActLoopCompletion {
    #[cfg(not(target_arch = "wasm32"))]
    done: Arc<native::Finished>,
    #[cfg(target_arch = "wasm32")]
    handle: Option<crate::runtime::worker_pool::TaskHandle<()>>,
}

impl ActLoopCompletion {
    /// Block until the act loop has finished: its stream ended, its tap was
    /// closed, or `head` cut it short.
    // Cost: O(1) plus the wait.
    pub(in crate::runtime) fn wait(self) {
        #[cfg(not(target_arch = "wasm32"))]
        self.done.wait();
        #[cfg(target_arch = "wasm32")]
        if let Some(handle) = self.handle {
            let _ = handle.join();
        }
    }
}

impl ActLoop {
    /// A loop calling `cb` on `interp` for every value, with no delay, no
    /// `done`/`quit` handlers, no close handle and no `.lines`/`.head` shaping;
    /// callers override the fields they need with struct-update syntax.
    pub(in crate::runtime) fn new(interp: Interpreter, cb: Value) -> Self {
        ActLoop {
            interp,
            cb,
            delay_seconds: 0.0,
            done_cb: None,
            quit_cb: None,
            close: None,
            is_lines: false,
            line_chomp: true,
            head_limit: None,
            producer_supplier_id: None,
            line_buffer: String::new(),
        }
    }

    fn is_closed(&self) -> bool {
        self.close
            .as_ref()
            .is_some_and(|(_, flag)| flag.load(Ordering::Acquire))
    }

    /// Start consuming `rx`. `gate` is the tap callback when the start must
    /// wait for the declaration it captures from its own initializer
    /// (`decl_gate`); `None` starts at once.
    // Cost: O(1) to start; each event later costs one drain-task submit at most.
    pub(in crate::runtime) fn start(
        self,
        rx: SupplyReceiver,
        gate: Option<&Value>,
    ) -> ActLoopCompletion {
        #[cfg(not(target_arch = "wasm32"))]
        {
            let (tap, first_drain) = native::ActTap::new(self, rx);
            let done = Arc::clone(&tap.finished);
            match gate {
                Some(cb) => crate::runtime::decl_gate::submit_after_declaration(cb, first_drain),
                None => crate::runtime::worker_pool::submit(first_drain),
            }
            ActLoopCompletion { done }
        }
        #[cfg(target_arch = "wasm32")]
        {
            let run = move || self.run_blocking(&rx);
            match gate {
                Some(cb) => {
                    crate::runtime::decl_gate::submit_after_declaration(cb, run);
                    ActLoopCompletion { handle: None }
                }
                None => ActLoopCompletion {
                    handle: Some(crate::runtime::worker_pool::submit_joinable(run)),
                },
            }
        }
    }

    /// The wasm32 consumer: one close check, then a plain blocking receive,
    /// per event.
    #[cfg(target_arch = "wasm32")]
    fn run_blocking(mut self, rx: &SupplyReceiver) {
        loop {
            if self.is_closed() {
                break;
            }
            let Ok(event) = rx.recv() else {
                break;
            };
            if self.handle_event(event) == Flow::Finish {
                break;
            }
        }
        self.retire();
    }

    /// Dispatch every event queued for this tap. `Continue` means the channel
    /// is empty and the loop waits for its next kick.
    // Cost: O(e * c), e = events queued, c = cost of the tap callback.
    #[cfg(not(target_arch = "wasm32"))]
    fn drain_available(&mut self, rx: &SupplyReceiver) -> Flow {
        loop {
            if self.is_closed() {
                return Flow::Finish;
            }
            match rx.try_recv_settled() {
                Ok(event) => {
                    if self.handle_event(event) == Flow::Finish {
                        return Flow::Finish;
                    }
                }
                Err(std::sync::mpsc::TryRecvError::Empty) => return Flow::Continue,
                Err(std::sync::mpsc::TryRecvError::Disconnected) => return Flow::Finish,
            }
        }
    }

    /// Drop the close registration: every exit path goes through here (the
    /// `process::exit` paths tear the whole process down anyway).
    fn retire(&mut self) {
        if let Some((id, _)) = self.close {
            super::state_lock::unregister_act_loop_close(id);
        }
    }

    /// Turn one received event into dispatch units and run them.
    ///
    /// A single event can expand into several units when `is_lines`: a chunk
    /// may complete more than one line (or none, if the buffer still holds a
    /// partial line), and a Done/Quit flushes any trailing partial line as one
    /// more unit before its done/quit callback. Done/Quit always finish the
    /// loop, whether or not their callback is present.
    fn handle_event(&mut self, event: SupplyEvent) -> Flow {
        let mut units: Vec<DispatchUnit> = Vec::new();
        let mut flow = Flow::Continue;
        match event {
            SupplyEvent::Emit(value) => {
                if self.is_lines {
                    self.line_buffer.push_str(&value.to_string_value());
                    for line in take_complete_lines_from_buffer(
                        &mut self.line_buffer,
                        self.line_chomp,
                        false,
                    ) {
                        units.push((Value::str(line), None, false));
                    }
                } else {
                    units.push((value, None, false));
                }
                if let Some(remaining) = self.head_limit {
                    if units.len() >= remaining {
                        units.truncate(remaining);
                        self.head_limit = Some(0);
                        flow = Flow::Finish;
                        if let Some(ref cb) = self.done_cb {
                            units.push((Value::NIL, Some((cb.clone(), Vec::new())), true));
                        }
                    } else {
                        self.head_limit = Some(remaining - units.len());
                    }
                }
            }
            SupplyEvent::Done => {
                flow = Flow::Finish;
                self.flush_partial_line(&mut units);
                if let Some(ref cb) = self.done_cb {
                    units.push((Value::NIL, Some((cb.clone(), Vec::new())), true));
                }
            }
            SupplyEvent::Quit(reason) => {
                flow = Flow::Finish;
                self.flush_partial_line(&mut units);
                if let Some(ref cb) = self.quit_cb {
                    units.push((Value::NIL, Some((cb.clone(), vec![reason])), false));
                }
            }
        }
        for unit in units {
            // Once `close` returns no new body dispatch may start (the pin
            // test t/supply-tap-close-interval.t relies on this being a hard
            // guarantee) — not even the rest of one chunk's lines.
            if self.is_closed() {
                return Flow::Finish;
            }
            if self.dispatch(unit) == Flow::Finish {
                return Flow::Finish;
            }
        }
        flow
    }

    fn flush_partial_line(&mut self, units: &mut Vec<DispatchUnit>) {
        if self.is_lines && !self.line_buffer.is_empty() {
            units.push((Value::str(std::mem::take(&mut self.line_buffer)), None, false));
        }
    }

    /// Run one unit on this tap's interpreter, flush its output, and decide
    /// what its outcome means for the loop.
    fn dispatch(&mut self, (value, end_cb, is_done_marker): DispatchUnit) -> Flow {
        use std::io::Write;
        Interpreter::sleep_for_supply_delay(self.delay_seconds);
        let interp = &mut self.interp;
        let cb = &self.cb;
        // A `done`/`last` thrown by the body is absorbed below — see
        // `runtime::react_done_handler_depth`.
        let react_done_handler =
            crate::runtime::react_done_handler_depth::ReactDoneHandlerGuard::new();
        // `guard_worker_panic` (issue #8185): this runs detached on a pooled
        // worker, whose `worker_loop` catches and *discards* an escaping panic
        // — a Rust panic raised outside the VM's own `run_inner_guarded` frames
        // therefore killed the tap silently. Converting it to the same
        // catchable `X::AdHoc` ("Internal error: ...") the VM boundary produces
        // puts it on the ordinary failure path below.
        //
        // `is_done_marker` selects `invoke_done_callback` over a plain call:
        // the tap's `done =>` slot may hold a done-group marker or a
        // `__SupplyDoneChain` (a `whenever`'s LAST phasers bundled with the
        // enclosing supply's group marker), which only that dispatcher
        // understands.
        let result = crate::vm::guard_worker_panic(|| match end_cb {
            Some((end, _)) if is_done_marker => interp.invoke_done_callback(end).map(|_| ()),
            Some((end, args)) => interp.call_sub_value(end, args).map(|_| ()),
            None => interp
                .call_sub_value(cb.clone(), vec![value])
                .map(|_| ()),
        });
        drop(react_done_handler);
        // Flush stdout (check both the per-interpreter buffer and the shared
        // thread output buffer used by thread clones), then stderr.
        if !interp.output_sink().output.is_empty() {
            print!("{}", interp.output_sink().output);
            let _ = std::io::stdout().flush();
            interp.output_sink_mut().output.clear();
        }
        if let Some(ref shared) = interp.output_sink().shared_thread_output {
            let drained = std::mem::take(&mut *shared.lock().unwrap());
            if !drained.is_empty() {
                print!("{}", drained);
                let _ = std::io::stdout().flush();
            }
        }
        if !interp.output_sink().stderr_output.is_empty() {
            eprint!("{}", interp.output_sink().stderr_output);
            let _ = std::io::stderr().flush();
            interp.output_sink_mut().stderr_output.clear();
        }
        if let Some(ref shared) = interp.output_sink().shared_thread_stderr {
            let drained = std::mem::take(&mut *shared.lock().unwrap());
            if !drained.is_empty() {
                eprint!("{}", drained);
                let _ = std::io::stderr().flush();
            }
        }
        // If the callback called exit, terminate the process (running the
        // main program's END phasers, ADR-11617).
        if interp.control.halted {
            let code = interp.control.exit_code;
            interp.exit_from_worker(code);
        }
        let Err(mut err) = result else {
            return Flow::Continue;
        };
        // A `done`/`last` is a control signal the supply machinery owns, not a
        // failure. It reaches here whenever the body (or the tap's `done =>`
        // chain) completes the supply from this act loop; every other supply
        // drive loop absorbs it the same way — see the `is_react_done() ||
        // is_last()` arms in `native_supply_mut_methods` and
        // `vm_react_subscriptions`. `is_supply_body_done()` is the same story
        // for a `whenever` body written directly inside a `supply { }` block
        // (its `done` desugars to this signal, see `ast::Stmt::SupplyBodyDone`).
        if err.is_react_done() || err.is_last() || err.is_supply_body_done() {
            return Flow::Finish;
        }
        // Producer code: its failure is the enclosing supply's quit, not an
        // unhandled crash of the process. Reached via the serialize-group link
        // for the same reason `invoke_supply_done_callback_for_supplier` is
        // (ADR-0031 Decision A): the downstream handler lives on the enclosing
        // supply block's emitter, not on this source.
        if let Some(sid) = self.producer_supplier_id {
            match Interpreter::deliver_act_loop_producer_quit(interp, sid, &err) {
                Ok(true) => return Flow::Finish,
                Ok(false) => {}
                // The quit handler itself failed: report *that*, loudly,
                // rather than losing both errors.
                Err(handler_err) => err = handler_err,
            }
        }
        eprintln!(
            "Unhandled exception in code scheduled on thread\n{}",
            err.message
        );
        let _ = std::io::stderr().flush();
        self.interp.exit_from_worker(1);
    }
}

#[cfg(not(target_arch = "wasm32"))]
mod native {
    use super::{ActLoop, Flow};
    use crate::runtime::native_methods::supply_channel::{DrainHook, SupplyReceiver};
    use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
    use std::sync::{Arc, Condvar, Mutex, MutexGuard};

    fn lock<T>(m: &Mutex<T>) -> MutexGuard<'_, T> {
        m.lock().unwrap_or_else(|e| e.into_inner())
    }

    /// Set once the act loop has retired, for [`super::ActLoopCompletion`].
    #[derive(Default)]
    pub(super) struct Finished {
        flag: Mutex<bool>,
        cv: Condvar,
    }

    impl Finished {
        fn set(&self) {
            *lock(&self.flag) = true;
            self.cv.notify_all();
        }

        pub(super) fn wait(&self) {
            let _ = crate::gc::wait_until(&self.flag, &self.cv, |done| *done);
        }
    }

    /// One tap's event-driven act loop (see the module docs).
    pub(super) struct ActTap {
        /// The loop and its receiver; `None` once retired. Locked only by the
        /// one drain holding `scheduled`.
        run: Mutex<Option<(ActLoop, SupplyReceiver)>>,
        /// Held by the drain that may run (or is queued to).
        scheduled: AtomicBool,
        /// A kick arrived since the running drain last read the channel.
        dirty: AtomicBool,
        hook_id: u64,
        pub(super) finished: Arc<Finished>,
    }

    impl ActTap {
        /// Build the tap and its first drain. `scheduled` starts held by that
        /// drain, so kicks arriving before it runs (a send, or a close while
        /// `decl_gate` holds the start) only mark the tap dirty.
        pub(super) fn new(
            act: ActLoop,
            rx: SupplyReceiver,
        ) -> (Arc<ActTap>, impl FnOnce() + Send + 'static) {
            static HOOK_IDS: AtomicU64 = AtomicU64::new(1);
            let close_id = act.close.as_ref().map(|(id, _)| *id);
            let tap = Arc::new(ActTap {
                run: Mutex::new(None),
                scheduled: AtomicBool::new(true),
                dirty: AtomicBool::new(false),
                hook_id: HOOK_IDS.fetch_add(1, Ordering::Relaxed),
                finished: Arc::new(Finished::default()),
            });
            // The hook keeps the tap alive while it is idle; `retire` breaks
            // that cycle (hook -> tap -> receiver -> broadcast -> hook).
            let kick: DrainHook = {
                let tap = Arc::clone(&tap);
                Arc::new(move || tap.kick())
            };
            rx.register_drain_hook(tap.hook_id, Arc::clone(&kick));
            if let Some(id) = close_id {
                crate::runtime::native_methods::state_lock::attach_act_loop_kick(id, kick);
            }
            *lock(&tap.run) = Some((act, rx));
            let first = Arc::clone(&tap);
            (tap, move || first.drain())
        }

        /// Note new work and make sure exactly one drain will see it.
        // Cost: O(1); one pool submit when no drain is scheduled.
        fn kick(self: &Arc<Self>) {
            self.dirty.store(true, Ordering::SeqCst);
            if !self.scheduled.swap(true, Ordering::SeqCst) {
                let me = Arc::clone(self);
                crate::runtime::worker_pool::submit(move || me.drain());
            }
        }

        /// Run with `scheduled` held: dispatch everything queued, then go idle.
        fn drain(self: Arc<Self>) {
            loop {
                self.dirty.store(false, Ordering::SeqCst);
                let flow = match lock(&self.run).as_mut() {
                    Some((act, rx)) => act.drain_available(rx),
                    // Already retired: `scheduled` stays held, so no drain
                    // is ever submitted again.
                    None => return,
                };
                if flow == Flow::Finish {
                    self.retire();
                    return;
                }
                self.scheduled.store(false, Ordering::SeqCst);
                // A kick between our last read and the release found
                // `scheduled` held and submitted nothing: re-claim and drain.
                if !self.dirty.load(Ordering::SeqCst) || self.scheduled.swap(true, Ordering::SeqCst)
                {
                    return;
                }
            }
        }

        fn retire(&self) {
            let taken = lock(&self.run).take();
            if let Some((mut act, rx)) = taken {
                rx.unregister_drain_hook(self.hook_id);
                act.retire();
                // Dropping the receiver releases this tap's queue, so a
                // producer whose last tap this was retires on its next send.
                drop(rx);
                drop(act);
            }
            self.finished.set();
        }
    }
}
