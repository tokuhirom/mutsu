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

    pub(in crate::runtime) fn native_semaphore(
        &mut self,
        attributes: &AttrMap,
        method: &str,
        _args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let sem_id = match attributes.get("semaphore-id").and_then(|v| v.as_int()) {
            Some(id) if id > 0 => id as u64,
            _ => {
                return Err(RuntimeError::new(format!(
                    "Semaphore.{} called on invalid Semaphore",
                    method
                )));
            }
        };
        let rt = semaphore_runtime_by_id(sem_id)
            .ok_or_else(|| RuntimeError::new("Semaphore state not found"))?;
        match method {
            "acquire" => {
                semaphore_acquire(&rt)?;
                // Entering the critical section: pull the latest value of any
                // shared scalar a previous holder committed inside its own
                // critical section, so `$r += $i` here reads the accumulated
                // value rather than this thread's stale clone-time copy.
                self.enter_critical_section();
                Ok(Value::NIL)
            }
            "try_acquire" => {
                let ok = semaphore_try_acquire(&rt)?;
                if ok {
                    self.enter_critical_section();
                }
                Ok(Value::truth(ok))
            }
            "release" => {
                self.leave_critical_section();
                semaphore_release(&rt)?;
                Ok(Value::NIL)
            }
            "WHAT" => Ok(Value::package(Symbol::intern("Semaphore"))),
            "Str" | "gist" => Ok(Value::str_from("Semaphore")),
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on Semaphore",
                method
            ))),
        }
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
                    && self.call_sub_value(test, Vec::new(), false)?.truthy()
                {
                    return Ok(Value::NIL);
                }
                let held_recursion = state.recursion;
                state.owner = None;
                state.recursion = 0;
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
                    drop(state);

                    let predicate_ok = if let Some(test) = maybe_test.clone() {
                        self.call_sub_value(test, Vec::new(), false)?.truthy()
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
            "status" => Ok(attributes
                .get("status")
                .cloned()
                .unwrap_or(Value::str_from("Planned"))),
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
            "keep" => {
                let value = args.into_iter().next().unwrap_or(Value::TRUE);
                if let Err(status) = shared.try_keep(value) {
                    let msg = format!(
                        "Cannot keep/break a Promise more than once (status: {})",
                        status
                    );
                    let mut attrs = HashMap::new();
                    attrs.insert("message".to_string(), Value::str(msg.clone()));
                    attrs.insert("promise".to_string(), Value::promise(shared.clone()));
                    let ex = Value::make_instance(Symbol::intern("X::Promise::Resolved"), attrs);
                    let mut err = RuntimeError::new(msg);
                    err.exception = Some(Box::new(ex));
                    return Err(err);
                }
                Ok(Value::NIL)
            }
            "break" => {
                let reason = args
                    .into_iter()
                    .next()
                    .unwrap_or_else(|| Value::str_from("Died"));
                if let Err(status) = shared.try_break(reason) {
                    let msg = format!(
                        "Cannot keep/break a Promise more than once (status: {})",
                        status
                    );
                    let mut attrs = HashMap::new();
                    attrs.insert("message".to_string(), Value::str(msg.clone()));
                    attrs.insert("promise".to_string(), Value::promise(shared.clone()));
                    let ex = Value::make_instance(Symbol::intern("X::Promise::Resolved"), attrs);
                    let mut err = RuntimeError::new(msg);
                    err.exception = Some(Box::new(ex));
                    return Err(err);
                }
                Ok(Value::NIL)
            }
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

    pub(in crate::runtime) fn native_thread(
        &mut self,
        attributes: &AttrMap,
        method: &str,
    ) -> Result<Value, RuntimeError> {
        match method {
            "finish" => self.dispatch_thread_finish(attributes),
            "id" => Ok(attributes
                .get("id")
                .or_else(|| attributes.get("thread_id"))
                .cloned()
                .unwrap_or(Value::int(0))),
            "name" => Ok(attributes
                .get("name")
                .cloned()
                .unwrap_or_else(|| Value::str_from("<anon>"))),
            "is-initial-thread" => {
                let is_initial = attributes
                    .get("is_initial")
                    .map(|v| v.truthy())
                    .unwrap_or(false);
                Ok(Value::truth(is_initial))
            }
            "app_lifetime" => Ok(attributes
                .get("app_lifetime")
                .cloned()
                .unwrap_or(Value::FALSE)),
            "WHAT" => Ok(Value::package(crate::symbol::Symbol::intern("Thread"))),
            "Str" | "gist" => {
                let id = attributes
                    .get("id")
                    .or_else(|| attributes.get("thread_id"))
                    .and_then(|v| v.as_int())
                    .unwrap_or(0);
                let name = attributes
                    .get("name")
                    .map(|v| v.to_string_value())
                    .unwrap_or_else(|| "<anon>".to_string());
                Ok(Value::str(format!("Thread<{id}>({name})")))
            }
            _ => Err(RuntimeError::new(format!(
                "No method '{}' on Thread",
                method
            ))),
        }
    }
}
