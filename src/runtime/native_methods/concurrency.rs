use crate::runtime::*;
use crate::symbol::Symbol;
use crate::value::ValueView;

use super::state_lock::*;
use crate::value::AttrMap;

impl Interpreter {
    pub(in crate::runtime) fn native_lock(
        &mut self,
        attributes: &AttrMap,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        match method {
            "protect" => {
                // `.protect` requires a single Callable block; a non-Callable
                // (e.g. `.protect: %()`) matches no candidate and must throw
                // X::Multi::NoMatch (roast .../multi-no-match.t).
                if args.len() != 1
                    || !matches!(args[0].view(), ValueView::Sub(..) | ValueView::WeakSub(..))
                {
                    return Err(
                        crate::runtime::methods_signature_errors::make_multi_no_match_error(
                            "protect",
                        ),
                    );
                }
                let lock_id = match attributes.get("lock-id").and_then(|v| v.as_int()) {
                    Some(id) if id > 0 => id as u64,
                    _ => {
                        return Err(RuntimeError::new(
                            "Lock.protect called on Lock without lock-id",
                        ));
                    }
                };
                let lock = lock_runtime_by_id(lock_id)
                    .ok_or_else(|| RuntimeError::new("Lock.protect could not find lock state"))?;
                let me = current_thread_id();
                acquire_lock(&lock, me)?;
                // Entering the critical section: pull the latest value of any
                // shared scalar a previous holder committed inside its own
                // critical section (mirrors Semaphore.acquire).
                self.enter_critical_section();
                let code = args.first().cloned().unwrap_or(Value::NIL);
                let result = self.call_protect_block(&code);
                self.leave_critical_section();
                let _ = release_lock(&lock, me);
                result
            }
            "lock" => {
                let lock_id = match attributes.get("lock-id").and_then(|v| v.as_int()) {
                    Some(id) if id > 0 => id as u64,
                    _ => {
                        return Err(RuntimeError::new("Lock.lock called on invalid Lock"));
                    }
                };
                let lock = lock_runtime_by_id(lock_id)
                    .ok_or_else(|| RuntimeError::new("Lock.lock could not find lock state"))?;
                let me = current_thread_id();
                // Lock::Async.lock() returns a Promise; plain Lock.lock() returns Nil.
                let is_async = attributes
                    .get("async")
                    .map(|v| matches!(v.view(), ValueView::Bool(true)))
                    .unwrap_or(false);
                if is_async {
                    let promise = async_acquire_lock(&lock, me)?;
                    Ok(Value::promise(promise))
                } else {
                    acquire_lock(&lock, me)?;
                    self.enter_critical_section();
                    Ok(Value::NIL)
                }
            }
            "unlock" => {
                let lock_id = match attributes.get("lock-id").and_then(|v| v.as_int()) {
                    Some(id) if id > 0 => id as u64,
                    _ => {
                        return Err(RuntimeError::new("Lock.unlock called on invalid Lock"));
                    }
                };
                let lock = lock_runtime_by_id(lock_id)
                    .ok_or_else(|| RuntimeError::new("Lock.unlock could not find lock state"))?;
                let is_async = attributes
                    .get("async")
                    .map(|v| matches!(v.view(), ValueView::Bool(true)))
                    .unwrap_or(false);
                if is_async {
                    async_release_lock(&lock)?;
                } else {
                    self.leave_critical_section();
                    let me = current_thread_id();
                    release_lock(&lock, me)?;
                }
                Ok(Value::NIL)
            }
            "condition" => {
                let lock_id = match attributes.get("lock-id").and_then(|v| v.as_int()) {
                    Some(id) if id > 0 => id as u64,
                    _ => return Err(RuntimeError::new("Lock.condition called on invalid Lock")),
                };
                let lock = lock_runtime_by_id(lock_id)
                    .ok_or_else(|| RuntimeError::new("Lock.condition could not find lock state"))?;
                let cond_id = next_condition_id();
                let _ = ensure_condition(&lock, cond_id).ok_or_else(|| {
                    RuntimeError::new("Lock.condition failed to create condition")
                })?;
                let mut attrs = HashMap::new();
                attrs.insert("lock-id".to_string(), Value::int(lock_id as i64));
                attrs.insert("cond-id".to_string(), Value::int(cond_id as i64));
                Ok(Value::make_instance(
                    Symbol::intern("Lock::ConditionVariable"),
                    attrs,
                ))
            }
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on Lock",
                method
            ))),
        }
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

    pub(in crate::runtime) fn native_condition_variable(
        &mut self,
        attributes: &AttrMap,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
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
        match method {
            "signal" => {
                cond.notify_one();
                Ok(Value::NIL)
            }
            "signal_all" => {
                cond.notify_all();
                Ok(Value::NIL)
            }
            "wait" => {
                let maybe_test = args.first().cloned();
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
                loop {
                    state = cond
                        .wait(state)
                        .map_err(|_| RuntimeError::new("Condition wait failed"))?;
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
                    super::thread_lock_count::note_lock_released();
                    lock.lock_cv.notify_one();
                }
            }
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on Lock::ConditionVariable",
                method
            ))),
        }
    }

    // --- Promise mutable ---

    pub(in crate::runtime) fn native_promise_mut(
        &mut self,
        mut attrs: AttrMap,
        method: &str,
        _args: Vec<Value>,
        _publish: &mut crate::runtime::native_methods::AttrPublisher<'_>,
    ) -> Result<(Value, AttrMap), RuntimeError> {
        match method {
            "keep" => {
                let value = _args.first().cloned().unwrap_or(Value::NIL);
                attrs.insert("result".to_string(), value);
                attrs.insert("status".to_string(), Value::str_from("Kept"));
                Ok((Value::NIL, attrs))
            }
            _ => Err(RuntimeError::new(format!(
                "No native mutable method '{}' on Promise",
                method
            ))),
        }
    }

    // --- Channel mutable ---

    pub(in crate::runtime) fn native_channel_mut(
        &mut self,
        mut attrs: AttrMap,
        method: &str,
        args: Vec<Value>,
        _publish: &mut crate::runtime::native_methods::AttrPublisher<'_>,
    ) -> Result<(Value, AttrMap), RuntimeError> {
        match method {
            "send" => {
                let value = args.first().cloned().unwrap_or(Value::NIL);
                let pushed = attrs.get_mut("queue").is_some_and(|q| {
                    q.with_array_mut(|items, _k| crate::gc::Gc::make_mut(items).push(value.clone()))
                        .is_some()
                });
                if !pushed {
                    attrs.insert("queue".to_string(), Value::array(vec![value]));
                }
                Ok((Value::NIL, attrs))
            }
            "receive" => {
                let mut value = Value::NIL;
                if let Some(q) = attrs.get_mut("queue")
                    && let Some(Some(removed)) = q.with_array_mut(|items, _k| {
                        (!items.is_empty()).then(|| crate::gc::Gc::make_mut(items).remove(0))
                    })
                {
                    value = removed;
                }
                Ok((value, attrs))
            }
            "close" => {
                attrs.insert("closed".to_string(), Value::TRUE);
                Ok((Value::NIL, attrs))
            }
            _ => Err(RuntimeError::new(format!(
                "No native mutable method '{}' on Channel",
                method
            ))),
        }
    }

    // --- Promise immutable ---

    pub(in crate::runtime) fn native_promise(
        &mut self,
        attributes: &AttrMap,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        match method {
            "result" => Ok(attributes.get("result").cloned().unwrap_or(Value::NIL)),
            "status" => Ok(Self::promise_status_value(
                &attributes
                    .get("status")
                    .map(|s| s.to_string_value())
                    .unwrap_or_default(),
            )),
            "then" => {
                let block = args.first().cloned().unwrap_or(Value::NIL);
                let status = attributes
                    .get("status")
                    .cloned()
                    .unwrap_or(Value::str_from("Planned"));
                if status.as_str() == Some("Kept") {
                    let value = attributes.get("result").cloned().unwrap_or(Value::NIL);
                    let result = self.call_sub_value(block, vec![value], true)?;
                    Ok(self.make_promise_instance("Kept", result))
                } else {
                    Ok(self.make_promise_instance("Planned", Value::NIL))
                }
            }
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on Promise",
                method
            ))),
        }
    }

    pub(in crate::runtime) fn native_promise_vow(
        &mut self,
        attributes: &AttrMap,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let promise = attributes
            .get("promise")
            .ok_or_else(|| RuntimeError::new("Promise::Vow missing promise"))?;
        let ValueView::Promise(shared) = promise.view() else {
            return Err(RuntimeError::new("Promise::Vow promise is not a Promise"));
        };
        match method {
            // A `Vow` resolves its promise unconditionally -- holding the vow
            // IS the permission -- so neither arm consults the vow flag; only
            // an already-settled promise is an error. Both share the one
            // `X::Promise::Resolved` builder so the message stays in a single
            // place (`format_exception_message`), rather than being baked into
            // a `message` attribute that would shadow it.
            "keep" => {
                let value = args.into_iter().next().unwrap_or(Value::TRUE);
                if let Err(status) = self.resolve_promise_dispatching(&shared, true, value, None)? {
                    return Err(Interpreter::promise_resolved_error(&shared, &status));
                }
                Ok(Value::NIL)
            }
            "break" => {
                let reason = args
                    .into_iter()
                    .next()
                    .unwrap_or_else(|| Value::str_from("Died"));
                if let Err(status) =
                    self.resolve_promise_dispatching(&shared, false, reason, None)?
                {
                    return Err(Interpreter::promise_resolved_error(&shared, &status));
                }
                Ok(Value::NIL)
            }
            // The body of the block a user scheduler is cued with for `start`
            // (see `promise_start_thunk`).
            "__mutsu_run_start" => {
                let block = args.into_iter().next().unwrap_or(Value::NIL);
                self.run_cued_start_body(&shared, block)
            }
            // The body of the block a user scheduler is cued with to dispatch
            // a resolution's subscribers (ADR-0105 D2).
            // Cost: O(s), s = the resolution's subscribers.
            "__mutsu_run_dispatch" => Ok(Interpreter::run_cued_dispatch(
                &args.into_iter().next().unwrap_or(Value::NIL),
            )),
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on Promise::Vow",
                method
            ))),
        }
    }

    // --- Channel immutable ---

    pub(in crate::runtime) fn native_channel(&self, attributes: &AttrMap, method: &str) -> Value {
        match method {
            "closed" => attributes.get("closed").cloned().unwrap_or(Value::FALSE),
            _ => Value::NIL,
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
