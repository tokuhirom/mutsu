use crate::runtime::*;
use crate::symbol::Symbol;
use crate::value::ValueView;

use super::state_lock::*;
use crate::value::AttrMap;

/// The lock a `Lock` (or `Lock::Async`) receiver stands for, by its `lock-id`.
// Cost: O(1), one hash lookup.
fn lock_of(
    attributes: &AttrMap,
    method: &str,
) -> Result<(u64, std::sync::Arc<LockRuntime>), RuntimeError> {
    let lock_id = match attributes.get("lock-id").and_then(|v| v.as_int()) {
        Some(id) if id > 0 => id as u64,
        _ => {
            return Err(RuntimeError::new(format!(
                "Lock.{method} called on invalid Lock"
            )));
        }
    };
    lock_runtime_by_id(lock_id)
        .map(|lock| (lock_id, lock))
        .ok_or_else(|| RuntimeError::new(format!("Lock.{method} could not find lock state")))
}

/// Whether the receiver is a `Lock::Async`.
// Cost: O(1).
fn is_async(attributes: &AttrMap) -> bool {
    attributes
        .get("async")
        .is_some_and(|v| matches!(v.view(), ValueView::Bool(true)))
}

impl Interpreter {
    /// `Lock.protect(&code)`: run `code` holding the lock.
    // Cost: O(1) plus the wait for the lock and the block's own cost.
    pub(crate) fn lock_protect(
        &mut self,
        attributes: &AttrMap,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        // `.protect` requires a single Callable block; a non-Callable
        // (e.g. `.protect: %()`) matches no candidate and must throw
        // X::Multi::NoMatch (roast .../multi-no-match.t).
        if args.len() != 1 || !matches!(args[0].view(), ValueView::Sub(..) | ValueView::WeakSub(..))
        {
            return Err(crate::runtime::methods_signature_errors::make_multi_no_match_error(
                "protect",
            ));
        }
        let (_, lock) = lock_of(attributes, "protect")?;
        let me = current_thread_id();
        acquire_lock(&lock, me)?;
        // Entering the critical section: pull the latest value of any
        // shared scalar a previous holder committed inside its own
        // critical section (mirrors Semaphore.acquire).
        self.enter_critical_section();
        let result = self.call_protect_block(&args[0]);
        self.leave_critical_section();
        let _ = release_lock(&lock, me);
        result
    }

    /// `Lock.lock`: take the lock; `Lock::Async.lock` answers a `Promise`
    /// kept once it is taken.
    // Cost: O(1) plus the wait for the lock.
    pub(crate) fn lock_lock(&mut self, attributes: &AttrMap) -> Result<Value, RuntimeError> {
        let (_, lock) = lock_of(attributes, "lock")?;
        let me = current_thread_id();
        if is_async(attributes) {
            return Ok(Value::promise(async_acquire_lock(&lock, me)?));
        }
        acquire_lock(&lock, me)?;
        self.enter_critical_section();
        Ok(Value::NIL)
    }

    /// `Lock.unlock`.
    // Cost: O(1).
    pub(crate) fn lock_unlock(&mut self, attributes: &AttrMap) -> Result<Value, RuntimeError> {
        let (_, lock) = lock_of(attributes, "unlock")?;
        if is_async(attributes) {
            async_release_lock(&lock)?;
        } else {
            self.leave_critical_section();
            release_lock(&lock, current_thread_id())?;
        }
        Ok(Value::NIL)
    }

    /// `Lock.condition`: a new condition variable of the lock.
    // Cost: O(1).
    pub(crate) fn lock_condition(attributes: &AttrMap) -> Result<Value, RuntimeError> {
        let (lock_id, lock) = lock_of(attributes, "condition")?;
        let cond_id = next_condition_id();
        let _ = ensure_condition(&lock, cond_id)
            .ok_or_else(|| RuntimeError::new("Lock.condition failed to create condition"))?;
        let mut attrs = HashMap::new();
        attrs.insert("lock-id".to_string(), Value::int(lock_id as i64));
        attrs.insert("cond-id".to_string(), Value::int(cond_id as i64));
        Ok(Value::make_instance(
            Symbol::intern("Lock::ConditionVariable"),
            attrs,
        ))
    }

    /// The native methods of a `Semaphore`: every one `Semaphore` declares is a
    /// row of the method table, reached through its owner (ADR-11276 §9.22).
    // Cost: O(1) to find the row, plus the handler's own cost.
    pub(in crate::runtime) fn native_semaphore(
        &mut self,
        attributes: &AttrMap,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        if let Some(result) = crate::builtins::method_table::invoke_owner(
            self,
            &["Semaphore"],
            method,
            &args,
            || Value::make_instance_without_destroy(Symbol::intern("Semaphore"), attributes.clone()),
        ) {
            return result;
        }
        match method {
            "WHAT" => Ok(Value::package(Symbol::intern("Semaphore"))),
            "Str" | "gist" => Ok(Value::str_from("Semaphore")),
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on Semaphore",
                method
            ))),
        }
    }

    /// The semaphore a `Semaphore` receiver stands for, by its `semaphore-id`.
    // Cost: O(1), two hash lookups.
    fn semaphore_of(
        attributes: &AttrMap,
        method: &str,
    ) -> Result<std::sync::Arc<SemaphoreRuntime>, RuntimeError> {
        let sem_id = match attributes.get("semaphore-id").and_then(|v| v.as_int()) {
            Some(id) if id > 0 => id as u64,
            _ => {
                return Err(RuntimeError::new(format!(
                    "Semaphore.{} called on invalid Semaphore",
                    method
                )));
            }
        };
        semaphore_runtime_by_id(sem_id).ok_or_else(|| RuntimeError::new("Semaphore state not found"))
    }

    /// `Semaphore.acquire`: block until a permit is free.
    // Cost: O(1) plus the wait for a permit.
    pub(crate) fn semaphore_acquire_method(
        &mut self,
        attributes: &AttrMap,
    ) -> Result<Value, RuntimeError> {
        let rt = Self::semaphore_of(attributes, "acquire")?;
        semaphore_acquire(&rt)?;
        // Entering the critical section: pull the latest value of any
        // shared scalar a previous holder committed inside its own
        // critical section, so `$r += $i` here reads the accumulated
        // value rather than this thread's stale clone-time copy.
        self.enter_critical_section();
        Ok(Value::NIL)
    }

    /// `Semaphore.try_acquire`: take a permit if one is free.
    // Cost: O(1).
    pub(crate) fn semaphore_try_acquire_method(
        &mut self,
        attributes: &AttrMap,
    ) -> Result<Value, RuntimeError> {
        let rt = Self::semaphore_of(attributes, "try_acquire")?;
        let ok = semaphore_try_acquire(&rt)?;
        if ok {
            self.enter_critical_section();
        }
        Ok(Value::truth(ok))
    }

    /// `Semaphore.release`: give a permit back.
    // Cost: O(1).
    pub(crate) fn semaphore_release_method(
        &mut self,
        attributes: &AttrMap,
    ) -> Result<Value, RuntimeError> {
        let rt = Self::semaphore_of(attributes, "release")?;
        self.leave_critical_section();
        semaphore_release(&rt)?;
        Ok(Value::NIL)
    }

    /// The lock and condition variable a `Lock::ConditionVariable` receiver
    /// stands for, by its `lock-id` and `cond-id`.
    // Cost: O(1), two hash lookups.
    fn condition_of(
        attributes: &AttrMap,
    ) -> Result<
        (
            u64,
            std::sync::Arc<LockRuntime>,
            std::sync::Arc<std::sync::Condvar>,
        ),
        RuntimeError,
    > {
        let lock_id = match attributes.get("lock-id").and_then(|v| v.as_int()) {
            Some(id) if id > 0 => id as u64,
            _ => return Err(RuntimeError::new("Condition variable has invalid lock-id")),
        };
        let cond_id = match attributes.get("cond-id").and_then(|v| v.as_int()) {
            Some(id) if id > 0 => id as u64,
            _ => return Err(RuntimeError::new("Condition variable has invalid cond-id")),
        };
        let lock = lock_runtime_by_id(lock_id)
            .ok_or_else(|| RuntimeError::new("Condition variable lock state not found"))?;
        let cond = ensure_condition(&lock, cond_id)
            .ok_or_else(|| RuntimeError::new("Condition variable state not found"))?;
        Ok((cond_id, lock, cond))
    }

    /// `Lock::ConditionVariable.signal` / `.signal_all`: wake one waiter, or
    /// every one.
    // Cost: O(1).
    pub(crate) fn condition_signal(attributes: &AttrMap, all: bool) -> Result<Value, RuntimeError> {
        let (cond_id, lock, cond) = Self::condition_of(attributes)?;
        // The count is bumped under the lock-state mutex, which a waiter
        // holds from its last look at the count until it parks, so a
        // signal is either counted before the waiter looks or notifies a
        // parked waiter.
        lock.state
            .lock()
            .map_err(|_| RuntimeError::new("Lock state is poisoned"))?
            .note_signal(cond_id);
        if all {
            cond.notify_all();
        } else {
            cond.notify_one();
        }
        Ok(Value::NIL)
    }

    /// `Lock::ConditionVariable.wait(&test?)`: give the lock up until a
    /// signal (and, with a test, until it holds), then take it again.
    // Cost: O(1) per wake plus the test's cost.
    pub(crate) fn condition_wait(
        &mut self,
        attributes: &AttrMap,
        test: Option<Value>,
    ) -> Result<Value, RuntimeError> {
        let (cond_id, lock, cond) = Self::condition_of(attributes)?;
            let maybe_test = test;
            let me = current_thread_id();
            let mut state = lock
                .state
                .lock()
                .map_err(|_| RuntimeError::new("Lock state is poisoned"))?;
            match state.owner {
                Some(owner) if owner == me => {}
                _ => {
                    return Err(RuntimeError::new(
                        "Condition.wait requires the current thread to hold the lock",
                    ));
                }
            }
            if let Some(test) = maybe_test.clone()
                && self.call_protect_block(&test)?.truthy()
            {
                return Ok(Value::NIL);
            }
            let held_recursion = state.recursion;
            state.owner = None;
            state.recursion = 0;
            super::thread_lock_count::note_lock_released();
            lock.lock_cv.notify_one();
            // The main program's thread re-checks for a worker's pending
            // `exit` every `exit_handover::POLL` (ADR-11617, #13052): the
            // signaller may be that very worker. It gives up without the
            // lock, so the caller unwinds instead of running on.
            let adopter = crate::exit_handover::is_adopter_thread();
            // The signal count as of the last look under the state mutex.
            let mut seen = state.signal_count(cond_id);
            loop {
                state = if adopter {
                    // A timeout is not a signal: wait again until one
                    // arrives or the handover does. A notification that
                    // lands as the poll times out is reported as a
                    // timeout, so the signal count decides (#13184).
                    loop {
                        let (s, timeout) = cond
                            .wait_timeout(state, crate::exit_handover::POLL)
                            .map_err(|_| RuntimeError::new("Condition wait failed"))?;
                        if crate::exit_handover::requested() {
                            return Err(RuntimeError::exit_handover_pending());
                        }
                        state = s;
                        if !timeout.timed_out() || state.signal_count(cond_id) != seen {
                            break state;
                        }
                    }
                } else {
                    cond.wait(state)
                        .map_err(|_| RuntimeError::new("Condition wait failed"))?
                };
                while state.owner.is_some() && state.owner != Some(me) {
                    state = lock
                        .lock_cv
                        .wait(state)
                        .map_err(|_| RuntimeError::new("Lock reacquire wait failed"))?;
                }
                state.owner = Some(me);
                state.recursion = held_recursion.max(1);
                super::thread_lock_count::note_lock_taken();
                drop(state);

                let predicate_ok = if let Some(test) = maybe_test.clone() {
                    let value = self.call_protect_block(&test)?;
                    value.truthy()
                } else {
                    true
                };
                if predicate_ok {
                    return Ok(Value::NIL);
                }
                state = lock
                    .state
                    .lock()
                    .map_err(|_| RuntimeError::new("Lock state is poisoned"))?;
                state.owner = None;
                state.recursion = 0;
                seen = state.signal_count(cond_id);
                super::thread_lock_count::note_lock_released();
                lock.lock_cv.notify_one();
            }
    }

    // --- Promise / Channel subclasses ---

    /// A built-in method of an instance of a user subclass of `Promise` or
    /// `Channel`, reached through the class's native methods: the rows answer
    /// it on the native promise or channel the instance carries (#13634).
    // Cost: O(1) to find the payload, plus the row's own cost.
    pub(in crate::runtime) fn native_promise_channel_payload(
        &mut self,
        attributes: &AttrMap,
        class: &str,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let key = if class == "Channel" {
            crate::builtins::method_table::promise_channel::CHANNEL_PAYLOAD
        } else {
            crate::builtins::method_table::promise_channel::PROMISE_PAYLOAD
        };
        let Some(payload) = attributes.get(key).cloned() else {
            return Err(crate::runtime::methods_signature_errors::make_method_not_found_error(
                method, class, false,
            ));
        };
        self.call_method_with_values(payload, method, args)
    }

    /// [`Self::native_promise_channel_payload`] for the mutating entry: the
    /// payload is shared, so the instance's attributes are unchanged.
    // Cost: O(1) to find the payload, plus the row's own cost.
    pub(in crate::runtime) fn native_promise_channel_payload_mut(
        &mut self,
        attrs: AttrMap,
        class: &str,
        method: &str,
        args: Vec<Value>,
    ) -> Result<(Value, AttrMap), RuntimeError> {
        let result = self.native_promise_channel_payload(&attrs, class, method, args)?;
        Ok((result, attrs))
    }

    /// The native methods of a `Promise::Vow` ([`Self::promise_vow_method`]).
    // Cost: O(1) to find the row, plus the handler's own cost.
    pub(in crate::runtime) fn native_promise_vow(
        &mut self,
        attributes: &AttrMap,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        self.promise_vow_method(attributes, method, args).unwrap_or_else(|| {
            Err(crate::runtime::methods_signature_errors::make_method_not_found_error(
                method,
                "Promise::Vow",
                false,
            ))
        })
    }

    /// The methods of a `Promise::Vow`: `keep` and `break` are rows of the
    /// method table, reached through their owner (ADR-11276 §9.118); the two
    /// internal names run the bodies a user scheduler is cued with. `None` for
    /// any other name.
    // Cost: O(1) to find the row, plus the handler's own cost.
    pub(in crate::runtime) fn promise_vow_method(
        &mut self,
        attributes: &AttrMap,
        method: &str,
        args: Vec<Value>,
    ) -> Option<Result<Value, RuntimeError>> {
        if let Some(result) = crate::builtins::method_table::invoke_owner(
            self,
            &["Promise::Vow"],
            method,
            &args,
            || Value::make_instance(Symbol::intern("Promise::Vow"), attributes.clone()),
        ) {
            return Some(result);
        }
        let promise = attributes.get("promise")?.clone();
        let ValueView::Promise(shared) = promise.view() else {
            return None;
        };
        let arg = args.into_iter().next().unwrap_or(Value::NIL);
        match method {
            // The body of the block a user scheduler is cued with for `start`
            // (see `promise_start_thunk`).
            // TODO: compile to bytecode -- the cue thunk should call a native
            // routine, not a method by name (ADR-11276 3G remainder, the
            // `__mutsu_*` names).
            "__mutsu_run_start" => Some(self.run_cued_start_body(&shared, arg)),
            // The body of the block a user scheduler is cued with to dispatch
            // a resolution's subscribers (ADR-0105 D2).
            // Cost: O(s), s = the resolution's subscribers.
            "__mutsu_run_dispatch" => Some(Ok(Interpreter::run_cued_dispatch(&arg))),
            _ => None,
        }
    }

    // --- Thread ---

    /// The native methods of a `Thread`: every one `Thread` declares is a row of
    /// the method table, reached through its owner (ADR-11276 §9.22).
    // Cost: O(1) to find the row, plus the handler's own cost.
    pub(in crate::runtime) fn native_thread(
        &mut self,
        attributes: &AttrMap,
        method: &str,
    ) -> Result<Value, RuntimeError> {
        if let Some(result) = crate::builtins::method_table::invoke_owner(
            self,
            &["Thread"],
            method,
            &[],
            || Value::make_instance_without_destroy(Symbol::intern("Thread"), attributes.clone()),
        ) {
            return result;
        }
        if method == "WHAT" {
            return Ok(Value::package(crate::symbol::Symbol::intern("Thread")));
        }
        Err(RuntimeError::new(format!(
            "No method '{}' on Thread",
            method
        )))
    }
}
