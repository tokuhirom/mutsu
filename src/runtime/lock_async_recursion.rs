//! `Lock::Async.protect-or-queue-on-recursion` and
//! `Lock::Async.with-lock-hidden-from-recursion-check`.
//!
//! `Lock::Async` is *not* re-entrant, so a `.protect` that runs while the same
//! caller chain already holds the lock deadlocks. These two methods exist to
//! make that shape expressible:
//!
//! - `protect-or-queue-on-recursion(&code)` behaves exactly like `.protect`
//!   when the lock is free (or held by something *outside* the caller chain),
//!   returning an undefined `Any`. When the caller chain already entered this
//!   same lock through `protect-or-queue-on-recursion`, it instead **queues**
//!   `&code` to run once the outer call has released the lock, and returns a
//!   `Promise` that is kept with the queued code's value.
//! - `with-lock-hidden-from-recursion-check(&code)` runs `&code` immediately
//!   with this lock temporarily removed from the recursion list, and returns
//!   the code's value. It never acquires the lock itself — verified against
//!   `raku` v2026.06: the call completes even while another thread holds the
//!   lock.
//!
//! The "caller chain" is modelled by [`Interpreter::lock_async_recursion`], a
//! per-interpreter stack of lock ids. A spawned thread gets a fresh (empty)
//! one from `clone_for_thread`, which is exactly the "locked by something
//! outside the caller chain" case the documentation describes.
//!
//! The deferred queue lives on the interpreter (not in a thread-local) so the
//! queued `Value`s are enumerated by [`Interpreter::visit_roots`] and cannot be
//! collected while they sit between the inner call that queued them and the
//! outer frame that drains them.

use crate::value::{RuntimeError, SharedPromise, Value};

use super::Interpreter;

impl Interpreter {
    /// `Lock::Async.protect-or-queue-on-recursion(&code)`.
    pub(crate) fn exec_lock_protect_or_queue_on_recursion(
        &mut self,
        lock_id: u64,
        code_val: Value,
    ) -> Result<Value, RuntimeError> {
        if self.lock_async_recursion.contains(&lock_id) {
            // Recursion on this lock within the current caller chain: queue the
            // code for the outer frame's drain and hand back a Promise.
            let promise = SharedPromise::new();
            self.lock_async_deferred
                .push((lock_id, code_val, promise.clone()));
            return Ok(Value::promise(promise));
        }

        let drain_base = self.lock_async_deferred.len();
        let result = self.run_lock_async_recursion_block(lock_id, &code_val);
        // Anything queued while this frame held the lock now runs, in FIFO
        // order, with the lock re-taken for each entry. A queued block may
        // itself queue more; those are appended past `drain_base` and picked up
        // by the same loop.
        while self.lock_async_deferred.len() > drain_base {
            let (queued_id, block, promise) = self.lock_async_deferred.remove(drain_base);
            match self.run_lock_async_recursion_block(queued_id, &block) {
                Ok(value) => {
                    let _ = promise.try_keep(value);
                }
                Err(err) => {
                    let reason = match err.exception {
                        Some(ex) => *ex,
                        None => Value::str(err.message.into_owned()),
                    };
                    let _ = promise.try_break(reason);
                }
            }
        }
        // The block's own failure is reported to *this* caller (like `.protect`),
        // but only after the queue has drained -- rakudo's `LEAVE self.unlock`
        // hands the lock on regardless of how the block ended.
        result?;
        Ok(Value::package(crate::symbol::Symbol::intern("Any")))
    }

    /// `Lock::Async.with-lock-hidden-from-recursion-check(&code)`: run `&code`
    /// with this lock hidden from the recursion list. Does not touch the lock.
    pub(crate) fn exec_lock_with_lock_hidden_from_recursion_check(
        &mut self,
        lock_id: u64,
        code_val: Value,
    ) -> Result<Value, RuntimeError> {
        let saved = self.lock_async_recursion.clone();
        self.lock_async_recursion.retain(|id| *id != lock_id);
        let result = self.call_sub_value(code_val, vec![], false);
        self.lock_async_recursion = saved;
        result
    }

    /// Run `code_val` under the lock, with `lock_id` marked as entered for the
    /// duration so a nested `protect-or-queue-on-recursion` sees the recursion.
    fn run_lock_async_recursion_block(
        &mut self,
        lock_id: u64,
        code_val: &Value,
    ) -> Result<Value, RuntimeError> {
        let lock = crate::runtime::native_methods::lock_runtime_by_id(lock_id)
            .ok_or_else(|| RuntimeError::new("Lock::Async method could not find lock state"))?;
        let me = crate::runtime::native_methods::current_thread_id();
        crate::runtime::native_methods::acquire_lock(&lock, me)?;
        self.enter_critical_section();
        self.lock_async_recursion.push(lock_id);
        let result = self.call_sub_value(code_val.clone(), vec![], false);
        self.lock_async_recursion.pop();
        self.leave_critical_section();
        let _ = crate::runtime::native_methods::release_lock(&lock, me);
        result
    }
}
