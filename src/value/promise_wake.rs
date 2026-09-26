//! How a resolved promise reaches its subscribers (ADR-0105 D2/D3).
//!
//! A promise's subscribers are its `.then`-style callbacks and the `await`s
//! parked on it, kept in one list in registration order. Resolving a promise
//! does not wake anyone by itself; what happens next depends on the promise's
//! scheduler:
//!
//! - **Built-in scheduler** (`scheduler == None`): callbacks run on one pooled
//!   task, as before, and every parked awaiter is granted its wake-up. When
//!   the resolving thread is a pool worker the grant is deferred to that
//!   worker's next yield (its next park, its task end, or the pool tick), so
//!   the woken thread never overtakes the keeper's own straight-line code —
//!   Rakudo's F3 ordering, without an extra queue hop (D2).
//! - **User scheduler**: an interpreter-aware resolving site gets the drained
//!   subscribers back as a [`UserDispatch`] and cues one task through the
//!   scheduler's `.cue(&dispatcher, :catch)` (F1). That task runs the
//!   subscribers in order; for a parked awaiter it grants the wake-up and then
//!   waits until the resumed awaiter reaches its next blocking point or task
//!   end (the D3 rendezvous). Completion of the cued task therefore means "the
//!   awaiter ran up to its next blocking point", which is what virtual-time
//!   schedulers such as `Test::Scheduler` build their ordering on.
//!
//! A resolving site with no interpreter at hand (a timer, an I/O reader, a
//! combinator waiter) takes the built-in path even for a user-scheduler
//! promise; nothing is ever stranded.

use super::*;
use crate::runtime::native_methods::SupplyTicket;
use std::cell::RefCell;

/// One entry of a promise's subscriber list, in registration order.
pub(crate) enum Subscriber {
    /// A `.then`-style callback, plus the serialize group of the supply block
    /// whose reaction it is, if any (see `SupplyTicket`).
    Callback(PromiseWaiter, Option<u64>),
    /// An `await` parked on the promise, by its wake ticket.
    Wake(u64),
}

/// The outcome a resolution hands to its subscribers.
#[derive(Clone)]
struct Outcome {
    status: String,
    result: Value,
    output: String,
    stderr: String,
}

/// A drained subscriber, ready to run: supply-block reactions have already
/// reserved their place in the block's queue on the resolving thread.
enum Ready {
    Callback(PromiseWaiter, Option<SupplyTicket>),
    Wake(u64),
}

fn reserve(subscribers: Vec<Subscriber>) -> Vec<Ready> {
    subscribers
        .into_iter()
        .map(|s| match s {
            Subscriber::Callback(waiter, group) => Ready::Callback(
                waiter,
                group.map(crate::runtime::native_methods::reserve_supply_serialize),
            ),
            Subscriber::Wake(ticket) => Ready::Wake(ticket),
        })
        .collect()
}

/// A resolution whose subscribers a user scheduler dispatches (ADR-0105 D2).
/// The resolving site cues [`UserDispatch::run`] through `scheduler`'s
/// `.cue`; if it cannot, [`UserDispatch::run_builtin`] is the fallback.
pub(crate) struct UserDispatch {
    promise: SharedPromise,
    scheduler: Value,
    subscribers: Vec<Ready>,
    outcome: Outcome,
}

impl UserDispatch {
    pub(crate) fn scheduler(&self) -> &Value {
        &self.scheduler
    }

    pub(crate) fn promise(&self) -> &SharedPromise {
        &self.promise
    }

    /// The dispatcher body, run inside the task the user scheduler was cued
    /// with: every subscriber in registration order, on this thread. A parked
    /// awaiter is granted its wake-up and then lent this task's turn until it
    /// next blocks (D3).
    // Cost: O(s), s = subscribers; each wake additionally waits for its
    // awaiter's next blocking point.
    pub(crate) fn run(self) {
        let Self {
            promise,
            subscribers,
            outcome,
            ..
        } = self;
        for sub in subscribers {
            match sub {
                Ready::Callback(waiter, ticket) => {
                    let _serialize_guard = ticket.map(|t| t.redeem());
                    waiter(
                        outcome.status.clone(),
                        outcome.result.clone(),
                        outcome.output.clone(),
                        outcome.stderr.clone(),
                    );
                }
                Ready::Wake(ticket) => promise.grant_with_rendezvous(ticket),
            }
        }
    }

    /// Dispatch as a built-in scheduler would: the fallback when the user
    /// scheduler's `.cue` could not be called.
    pub(crate) fn run_builtin(self) {
        SharedPromise::dispatch_builtin(&self.promise, self.subscribers, self.outcome);
    }
}

/// User dispatches handed to a scheduler, waiting for their cued task to run.
/// The cued block names its dispatch by id, since a Raku block cannot carry
/// the Rust closures of the subscriber list.
fn registry() -> &'static Mutex<HashMap<u64, UserDispatch>> {
    static REGISTRY: std::sync::OnceLock<Mutex<HashMap<u64, UserDispatch>>> =
        std::sync::OnceLock::new();
    REGISTRY.get_or_init(|| Mutex::new(HashMap::new()))
}

/// Park `dispatch` until its cued task claims it with [`take_user_dispatch`].
// Cost: O(1) amortized.
pub(crate) fn register_user_dispatch(dispatch: UserDispatch) -> u64 {
    static NEXT: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(1);
    let id = NEXT.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    registry()
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .insert(id, dispatch);
    id
}

/// Claim a registered dispatch. `None` when the scheduler ran the task twice.
// Cost: O(1) amortized.
pub(crate) fn take_user_dispatch(id: u64) -> Option<UserDispatch> {
    registry()
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .remove(&id)
}

/// `MUTSU_TRACE=pool`: name a D3 rendezvous still open after 5s — the
/// signature of a resumed awaiter that spins without ever blocking (ADR-0105
/// §5). The watchdog is a plain thread that touches no `Gc` state.
#[cfg(not(target_arch = "wasm32"))]
fn rendezvous_watchdog(ticket: u64) -> std::sync::Arc<std::sync::atomic::AtomicBool> {
    let done = std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false));
    let flag = done.clone();
    let dispatcher = std::thread::current().id();
    let _ = std::thread::Builder::new()
        .name("pool-rendezvous-trace".to_string())
        .spawn(move || {
            std::thread::sleep(std::time::Duration::from_secs(5));
            if !flag.load(std::sync::atomic::Ordering::Acquire) {
                eprintln!(
                    "[TRACE:pool] user-scheduler wake dispatcher {dispatcher:?} has waited 5s \
                     for the awaiter holding ticket {ticket} to reach its next blocking point \
                     (ADR-0105 D3)"
                );
            }
        });
    done
}

/// Awaiters this thread resumed from a user-scheduler wake-up, whose
/// dispatcher is waiting for this thread's next blocking point (D3).
struct Borrowed(RefCell<Vec<(SharedPromise, u64)>>);

impl Drop for Borrowed {
    fn drop(&mut self) {
        // Last resort: every registered thread releases at its task end or
        // thread exit (`drop_thread_local_gc_state`) before it unregisters,
        // so this only sees threads that never registered.
        for (promise, ticket) in self.0.take() {
            promise.release_rendezvous(ticket);
        }
    }
}

thread_local! {
    static BORROWED: Borrowed = const { Borrowed(RefCell::new(Vec::new())) };
}

/// This thread reached a blocking point or the end of its task: release every
/// user-scheduler dispatcher that lent it its turn (ADR-0105 D3).
// Cost: O(b), b = rendezvous this thread owes (almost always 0).
pub(crate) fn release_borrowed_wakes() {
    let owed = BORROWED
        .try_with(|b| {
            if b.0.borrow().is_empty() {
                Vec::new()
            } else {
                b.0.take()
            }
        })
        .unwrap_or_default();
    for (promise, ticket) in owed {
        promise.release_rendezvous(ticket);
    }
}

impl SharedPromise {
    pub(crate) fn keep(&self, result: Value, output: String, stderr: String) {
        let _ = self.resolve("Kept", result, Some((output, stderr)), false, false);
    }

    /// Try to keep; returns Err(current_status) if already kept/broken.
    pub(crate) fn try_keep(&self, result: Value) -> Result<(), String> {
        self.resolve("Kept", result, None, true, false).map(|_| ())
    }

    pub(crate) fn break_with(&self, error: Value, output: String, stderr: String) {
        let _ = self.resolve("Broken", error, Some((output, stderr)), false, false);
    }

    /// Try to break; returns Err(current_status) if already kept/broken.
    pub(crate) fn try_break(&self, error: Value) -> Result<(), String> {
        self.resolve("Broken", error, None, true, false).map(|_| ())
    }

    /// [`Self::try_keep`] / [`Self::try_break`] (or, given `output`,
    /// [`Self::keep`] / [`Self::break_with`]) for a resolving site that can
    /// call into a user scheduler: when this promise is bound to one and has
    /// subscribers, they come back as a [`UserDispatch`] for the caller to cue
    /// (ADR-0105 D2) instead of being dispatched here.
    // Cost: O(s), s = subscribers.
    pub(crate) fn resolve_for_dispatch(
        &self,
        kept: bool,
        value: Value,
        output: Option<(String, String)>,
    ) -> Result<Option<UserDispatch>, String> {
        let status = if kept { "Kept" } else { "Broken" };
        let only_planned = output.is_none();
        self.resolve(status, value, output, only_planned, true)
    }

    /// The one resolution routine. `output` replaces the captured output when
    /// given; `only_planned` refuses an already-resolved promise;
    /// `for_dispatch` hands a user-scheduler resolution back to the caller.
    fn resolve(
        &self,
        status: &str,
        value: Value,
        output: Option<(String, String)>,
        only_planned: bool,
        for_dispatch: bool,
    ) -> Result<Option<UserDispatch>, String> {
        let (lock, _) = &*self.inner;
        let mut state = lock.lock().unwrap();
        if only_planned && state.status != "Planned" {
            return Err(state.status.clone());
        }
        state.status = status.to_string();
        state.result = value.clone();
        let (output, stderr) = match output {
            Some((output, stderr)) => {
                state.output = output.clone();
                state.stderr_output = stderr.clone();
                (output, stderr)
            }
            None => (String::new(), String::new()),
        };
        let subscribers = std::mem::take(&mut state.waiters);
        let scheduler = if for_dispatch && !subscribers.is_empty() {
            state.scheduler.clone()
        } else {
            None
        };
        drop(state);
        let outcome = Outcome {
            status: status.to_string(),
            result: value,
            output,
            stderr,
        };
        let subscribers = reserve(subscribers);
        match scheduler {
            Some(scheduler) => Ok(Some(UserDispatch {
                promise: self.clone(),
                scheduler,
                subscribers,
                outcome,
            })),
            None => {
                Self::dispatch_builtin(self, subscribers, outcome);
                Ok(None)
            }
        }
    }

    /// The built-in dispatch: callbacks on one pooled task in registration
    /// order, and every parked awaiter granted its wake-up — deferred to the
    /// resolving worker's next yield when the resolver is a pool worker
    /// (ADR-0105 D2), immediately otherwise.
    fn dispatch_builtin(promise: &SharedPromise, subscribers: Vec<Ready>, outcome: Outcome) {
        let mut has_wake = false;
        let mut callbacks = Vec::new();
        for sub in subscribers {
            match sub {
                Ready::Callback(waiter, ticket) => callbacks.push((waiter, ticket)),
                Ready::Wake(_) => has_wake = true,
            }
        }
        if has_wake {
            let promise = promise.clone();
            let grant: Box<dyn FnOnce() + Send> = Box::new(move || promise.grant_all());
            if let Err(grant) = crate::runtime::worker_pool::defer_until_yield(grant) {
                grant();
            }
        }
        if callbacks.is_empty() {
            return;
        }
        // Pooled (ADR-0020 slice 3). Ordering is preserved — all of this
        // promise's callbacks run in registration order inside the single
        // pooled task, and supply-block reactions took their place in the
        // block's queue on the resolving thread (#7811).
        crate::runtime::worker_pool::submit(move || {
            for (waiter, ticket) in callbacks {
                // Held across the callback; the `emit` inside it re-enters the
                // same group on this thread, which the lock allows.
                let _serialize_guard = ticket.map(|t| t.redeem());
                waiter(
                    outcome.status.clone(),
                    outcome.result.clone(),
                    outcome.output.clone(),
                    outcome.stderr.clone(),
                );
            }
        });
    }

    /// Wake every awaiter parked on this promise (the built-in grant).
    fn grant_all(&self) {
        let (lock, cvar) = &*self.inner;
        let mut state = lock.lock().unwrap();
        state.wake_granted = u64::MAX;
        cvar.notify_all();
    }

    /// Wake the awaiter holding `ticket` on behalf of a user scheduler's
    /// dispatch, then wait until it reaches its next blocking point or task
    /// end (ADR-0105 D3). Unbounded by design: a bound would make the ordering
    /// guarantee load-dependent. `MUTSU_TRACE=pool` names a rendezvous still
    /// open after 5s.
    fn grant_with_rendezvous(&self, ticket: u64) {
        let (lock, cvar) = &*self.inner;
        {
            let mut state = lock.lock().unwrap();
            state.wake_granted = state.wake_granted.max(ticket + 1);
            // wasm32: the cooperative pump is already sequential, and a
            // rendezvous wait there would pump the awaiter from inside this
            // task instead of letting it run.
            state.wake_rendezvous = cfg!(not(target_arch = "wasm32"));
            cvar.notify_all();
        }
        #[cfg(not(target_arch = "wasm32"))]
        {
            let watchdog = crate::trace::is_enabled("pool").then(|| rendezvous_watchdog(ticket));
            let _ = crate::gc::wait_until(lock, cvar, |s| s.wake_resumed > ticket);
            if let Some(done) = watchdog {
                done.store(true, std::sync::atomic::Ordering::Release);
            }
        }
    }

    /// The awaiter holding `ticket` reached its next blocking point.
    fn release_rendezvous(&self, ticket: u64) {
        let (lock, cvar) = &*self.inner;
        let mut state = lock.lock().unwrap();
        state.wake_resumed = state.wake_resumed.max(ticket + 1);
        cvar.notify_all();
    }

    /// Register a callback to run once this promise resolves. If it is
    /// already resolved, the callback runs synchronously on the caller's
    /// thread (matching the existing `.then`/`.andthen`/`.orelse` fast path
    /// for an already-kept/broken promise) and this returns `true`.
    /// (A supply-block reaction is the exception — see
    /// [`Self::on_resolve_in_supply_group`].)
    /// Otherwise the callback is queued and this returns `false`: the
    /// resolution runs every queued subscriber **in registration order**
    /// (see the module docs for on which thread). Registering into this
    /// shared, ordered queue (rather than having each caller spawn its own
    /// thread that blocks on `wait()`) is what makes sibling callbacks on the
    /// same promise (e.g. an independent `.then` alongside an `.andthen`
    /// chain) deterministically ordered instead of racing each other's OS
    /// thread wake-up latency.
    pub(crate) fn on_resolve(&self, waiter: PromiseWaiter) -> bool {
        self.on_resolve_in_supply_group(waiter, None)
    }

    /// [`Self::on_resolve`], for a waiter whose effect is a reaction of the
    /// supply block whose serialize group is `group`.
    ///
    /// Resolving the promise then reserves this reaction's place in that group
    /// **on the resolving thread**, before the pooled worker that will run it
    /// has been woken. Two promises resolved one after another therefore reach
    /// the supply block in that order, instead of in whichever order their two
    /// workers happened to wake up in. See [`SupplyTicket`].
    ///
    /// An **already-resolved** promise takes its ticket here instead, on the
    /// registering thread, and its reaction is handed to a pooled worker just
    /// like a queued one. Both paths therefore enter the block through the one
    /// sequencer, in the order the reactions were created. Running the reaction
    /// inline, as [`Self::on_resolve`] does for a plain waiter, would let it
    /// jump ahead of every reaction already ticketed and waiting for a worker
    /// (#7831); redeeming the ticket inline instead is not an option either,
    /// because this thread is typically running the enclosing supply block's
    /// own reaction and so already holds the group lock, so parking it behind
    /// an earlier ticket — whose worker is itself blocked on that lock — would
    /// deadlock. So this returns `false` (the reaction has not run yet) even
    /// when the promise was already resolved.
    pub(crate) fn on_resolve_in_supply_group(
        &self,
        waiter: PromiseWaiter,
        group: Option<u64>,
    ) -> bool {
        let (lock, _) = &*self.inner;
        let mut state = lock.lock().unwrap();
        if state.status == "Planned" {
            state.waiters.push(Subscriber::Callback(waiter, group));
            false
        } else {
            let status = state.status.clone();
            let result = state.result.clone();
            let output = state.output.clone();
            let stderr = state.stderr_output.clone();
            drop(state);
            let Some(group) = group else {
                waiter(status, result, output, stderr);
                return true;
            };
            let ticket = crate::runtime::native_methods::reserve_supply_serialize(group);
            crate::runtime::worker_pool::submit(move || {
                // Held across the callback, exactly as in `dispatch_builtin`.
                let _serialize_guard = ticket.redeem();
                waiter(status, result, output, stderr);
            });
            false
        }
    }

    /// Check if promise is resolved (Kept or Broken).
    pub(crate) fn is_resolved(&self) -> bool {
        let (lock, _) = &*self.inner;
        lock.lock().unwrap().status != "Planned"
    }

    pub(crate) fn status(&self) -> String {
        let (lock, _) = &*self.inner;
        lock.lock().unwrap().status.clone()
    }

    /// Block until this promise is resolved and this awaiter has been granted
    /// its wake-up, then return (result, output, stderr). An already-resolved
    /// promise returns at once, without a wake-up of its own (Rakudo's
    /// `$handle.already`).
    // Cost: O(1) plus the wait.
    pub(crate) fn wait(&self) -> (Value, String, String) {
        self.mark_observed();
        // GC safepoint (§9.2a `await`): the await entry boundary, before the
        // state lock is taken (a collect here can run finalizers that touch
        // other promises/channels, so it must not hold this mutex).
        crate::vm::vm_poll::poll(crate::gc::SafepointKind::Await, 0);
        let (lock, cvar) = &*self.inner;
        let ticket = {
            let mut state = lock.lock().unwrap();
            if state.status != "Planned" {
                return (
                    state.result.clone(),
                    state.output.clone(),
                    state.stderr_output.clone(),
                );
            }
            let ticket = state.wake_next;
            state.wake_next += 1;
            state.waiters.push(Subscriber::Wake(ticket));
            ticket
        };
        // STW-aware: the waiting thread counts as quiescent for the GC's
        // cooperative stop-the-world, and never resumes (cloning `Value`s
        // below mutates Gc refcounts) while a cycle scan is in progress.
        // On wasm this pumps the cooperative scheduler instead of parking —
        // the `start` block we are waiting for only runs because of it.
        let state = match crate::gc::wait_until(lock, cvar, |s| s.wake_granted > ticket) {
            Some(state) => state,
            None => {
                // Single-threaded build with nothing left to run: break the
                // promise so `await` reports a deadlock instead of hanging.
                let _ = self.try_break(Value::str(crate::gc::DEADLOCK_MESSAGE.to_string()));
                crate::gc::wait_until(lock, cvar, |s| s.wake_granted > ticket)
                    .unwrap_or_else(|| lock.lock().unwrap())
            }
        };
        let owes_rendezvous = state.wake_rendezvous;
        let out = (
            state.result.clone(),
            state.output.clone(),
            state.stderr_output.clone(),
        );
        drop(state);
        if owes_rendezvous {
            let promise = self.clone();
            let _ = BORROWED.try_with(|b| b.0.borrow_mut().push((promise, ticket)));
        }
        out
    }
}
