//! `await` on a promise: parking for a wake-up, and the ordering an awaiter
//! that arrives after the resolution still owes the resolving worker
//! (ADR-0105 D2). The resolution side is `promise_wake`.

use super::promise_wake::{BORROWED, Subscriber};
use super::*;

impl SharedPromise {
    /// The worker that resolved this promise yielded: release every `await`
    /// that reached it late (#10016).
    fn release_late_awaiters(&self) {
        let (lock, cvar) = &*self.inner;
        let mut state = lock.lock().unwrap();
        state.keeper_yielded = true;
        cvar.notify_all();
    }

    /// Block until this promise is resolved and this awaiter has been granted
    /// its wake-up, then return (result, output, stderr). An already-resolved
    /// promise returns without a wake-up of its own (Rakudo's
    /// `$handle.already`) once the pool worker that resolved it, if any, has
    /// yielded since (see `wait_for_keeper`).
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
                if !state.keeper_yielded
                    && let Some(keeper) = state.keeper.clone()
                {
                    drop(state);
                    return self.wait_for_keeper(keeper);
                }
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

    /// `await` on a promise whose resolving pool worker may not have yielded
    /// since: return only once it has (ADR-0105 D2), so an awaiter arriving
    /// after the keep observes the keeper's straight-line code just as a
    /// parked one does. Rakudo returns at once here (`$handle.already`), but
    /// its main thread reaches the `await` long before a pooled keeper does;
    /// an interpreted, preempted awaiter need not (#10016).
    // Cost: O(1) plus the wait.
    fn wait_for_keeper(
        &self,
        keeper: crate::runtime::worker_pool::KeeperMark,
    ) -> (Value, String, String) {
        let promise = self.clone();
        let release: Box<dyn FnOnce() + Send> = Box::new(move || promise.release_late_awaiters());
        let (lock, cvar) = &*self.inner;
        // Handed back: the keeper already yielded, or this thread is the
        // keeper. Either way its straight-line code is behind us.
        let state = match keeper.defer(release) {
            Ok(()) => crate::gc::wait_until(lock, cvar, |s| s.keeper_yielded)
                .unwrap_or_else(|| lock.lock().unwrap()),
            Err(_) => lock.lock().unwrap(),
        };
        (
            state.result.clone(),
            state.output.clone(),
            state.stderr_output.clone(),
        )
    }
}
