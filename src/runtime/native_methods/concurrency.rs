use crate::runtime::*;
use crate::symbol::Symbol;

use super::state_lock::*;

impl Interpreter {
    pub(in crate::runtime) fn native_lock(
        &mut self,
        attributes: &HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        match method {
            "protect" => {
                let lock_id = match attributes.get("lock-id") {
                    Some(Value::Int(id)) if *id > 0 => *id as u64,
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
                let code = args.first().cloned().unwrap_or(Value::Nil);
                let result = self.call_protect_block(&code);
                let _ = release_lock(&lock, me);
                result
            }
            "lock" => {
                let lock_id = match attributes.get("lock-id") {
                    Some(Value::Int(id)) if *id > 0 => *id as u64,
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
                    .map(|v| matches!(v, Value::Bool(true)))
                    .unwrap_or(false);
                if is_async {
                    let promise = async_acquire_lock(&lock, me)?;
                    Ok(Value::Promise(promise))
                } else {
                    acquire_lock(&lock, me)?;
                    Ok(Value::Nil)
                }
            }
            "unlock" => {
                let lock_id = match attributes.get("lock-id") {
                    Some(Value::Int(id)) if *id > 0 => *id as u64,
                    _ => {
                        return Err(RuntimeError::new("Lock.unlock called on invalid Lock"));
                    }
                };
                let lock = lock_runtime_by_id(lock_id)
                    .ok_or_else(|| RuntimeError::new("Lock.unlock could not find lock state"))?;
                let is_async = attributes
                    .get("async")
                    .map(|v| matches!(v, Value::Bool(true)))
                    .unwrap_or(false);
                if is_async {
                    async_release_lock(&lock)?;
                } else {
                    let me = current_thread_id();
                    release_lock(&lock, me)?;
                }
                Ok(Value::Nil)
            }
            "condition" => {
                let lock_id = match attributes.get("lock-id") {
                    Some(Value::Int(id)) if *id > 0 => *id as u64,
                    _ => return Err(RuntimeError::new("Lock.condition called on invalid Lock")),
                };
                let lock = lock_runtime_by_id(lock_id)
                    .ok_or_else(|| RuntimeError::new("Lock.condition could not find lock state"))?;
                let cond_id = next_condition_id();
                let _ = ensure_condition(&lock, cond_id).ok_or_else(|| {
                    RuntimeError::new("Lock.condition failed to create condition")
                })?;
                let mut attrs = HashMap::new();
                attrs.insert("lock-id".to_string(), Value::Int(lock_id as i64));
                attrs.insert("cond-id".to_string(), Value::Int(cond_id as i64));
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
        attributes: &HashMap<String, Value>,
        method: &str,
        _args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let sem_id = match attributes.get("semaphore-id") {
            Some(Value::Int(id)) if *id > 0 => *id as u64,
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
                Ok(Value::Nil)
            }
            "try_acquire" => {
                let ok = semaphore_try_acquire(&rt)?;
                Ok(Value::Bool(ok))
            }
            "release" => {
                semaphore_release(&rt)?;
                Ok(Value::Nil)
            }
            "WHAT" => Ok(Value::Package(Symbol::intern("Semaphore"))),
            "Str" | "gist" => Ok(Value::str_from("Semaphore")),
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on Semaphore",
                method
            ))),
        }
    }

    pub(in crate::runtime) fn native_condition_variable(
        &mut self,
        attributes: &HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let lock_id = match attributes.get("lock-id") {
            Some(Value::Int(id)) if *id > 0 => *id as u64,
            _ => return Err(RuntimeError::new("Condition variable has invalid lock-id")),
        };
        let cond_id = match attributes.get("cond-id") {
            Some(Value::Int(id)) if *id > 0 => *id as u64,
            _ => return Err(RuntimeError::new("Condition variable has invalid cond-id")),
        };
        let lock = lock_runtime_by_id(lock_id)
            .ok_or_else(|| RuntimeError::new("Condition variable lock state not found"))?;
        let cond = ensure_condition(&lock, cond_id)
            .ok_or_else(|| RuntimeError::new("Condition variable state not found"))?;
        match method {
            "signal" => {
                cond.notify_one();
                Ok(Value::Nil)
            }
            "signal_all" => {
                cond.notify_all();
                Ok(Value::Nil)
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
                    return Ok(Value::Nil);
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
                        return Ok(Value::Nil);
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
        mut attrs: HashMap<String, Value>,
        method: &str,
        _args: Vec<Value>,
    ) -> Result<(Value, HashMap<String, Value>), RuntimeError> {
        match method {
            "keep" => {
                let value = _args.first().cloned().unwrap_or(Value::Nil);
                attrs.insert("result".to_string(), value);
                attrs.insert("status".to_string(), Value::str_from("Kept"));
                Ok((Value::Nil, attrs))
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
        mut attrs: HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Result<(Value, HashMap<String, Value>), RuntimeError> {
        match method {
            "send" => {
                let value = args.first().cloned().unwrap_or(Value::Nil);
                match attrs.get_mut("queue") {
                    Some(Value::Array(items, ..)) => Arc::make_mut(items).push(value),
                    _ => {
                        attrs.insert("queue".to_string(), Value::array(vec![value]));
                    }
                }
                Ok((Value::Nil, attrs))
            }
            "receive" => {
                let mut value = Value::Nil;
                if let Some(Value::Array(items, ..)) = attrs.get_mut("queue")
                    && !items.is_empty()
                {
                    value = Arc::make_mut(items).remove(0);
                }
                Ok((value, attrs))
            }
            "close" => {
                attrs.insert("closed".to_string(), Value::Bool(true));
                Ok((Value::Nil, attrs))
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
        attributes: &HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        match method {
            "result" => Ok(attributes.get("result").cloned().unwrap_or(Value::Nil)),
            "status" => Ok(attributes
                .get("status")
                .cloned()
                .unwrap_or(Value::str_from("Planned"))),
            "then" => {
                let block = args.first().cloned().unwrap_or(Value::Nil);
                let status = attributes
                    .get("status")
                    .cloned()
                    .unwrap_or(Value::str_from("Planned"));
                if matches!(status, Value::Str(ref s) if s.as_str() == "Kept") {
                    let value = attributes.get("result").cloned().unwrap_or(Value::Nil);
                    let result = self.call_sub_value(block, vec![value], true)?;
                    Ok(self.make_promise_instance("Kept", result))
                } else {
                    Ok(self.make_promise_instance("Planned", Value::Nil))
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
        attributes: &HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let promise = attributes
            .get("promise")
            .ok_or_else(|| RuntimeError::new("Promise::Vow missing promise"))?;
        let Value::Promise(shared) = promise else {
            return Err(RuntimeError::new("Promise::Vow promise is not a Promise"));
        };
        match method {
            "keep" => {
                let value = args.into_iter().next().unwrap_or(Value::Bool(true));
                if let Err(status) = shared.try_keep(value) {
                    let msg = format!(
                        "Cannot keep/break a Promise more than once (status: {})",
                        status
                    );
                    let mut attrs = HashMap::new();
                    attrs.insert("message".to_string(), Value::str(msg.clone()));
                    attrs.insert("promise".to_string(), Value::Promise(shared.clone()));
                    let ex = Value::make_instance(Symbol::intern("X::Promise::Resolved"), attrs);
                    let mut err = RuntimeError::new(msg);
                    err.exception = Some(Box::new(ex));
                    return Err(err);
                }
                Ok(Value::Nil)
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
                    attrs.insert("promise".to_string(), Value::Promise(shared.clone()));
                    let ex = Value::make_instance(Symbol::intern("X::Promise::Resolved"), attrs);
                    let mut err = RuntimeError::new(msg);
                    err.exception = Some(Box::new(ex));
                    return Err(err);
                }
                Ok(Value::Nil)
            }
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on Promise::Vow",
                method
            ))),
        }
    }

    // --- Channel immutable ---

    pub(in crate::runtime) fn native_channel(
        &self,
        attributes: &HashMap<String, Value>,
        method: &str,
    ) -> Value {
        match method {
            "closed" => attributes
                .get("closed")
                .cloned()
                .unwrap_or(Value::Bool(false)),
            _ => Value::Nil,
        }
    }

    // --- Thread ---

    pub(in crate::runtime) fn native_thread(
        &mut self,
        attributes: &HashMap<String, Value>,
        method: &str,
    ) -> Result<Value, RuntimeError> {
        match method {
            "finish" => self.dispatch_thread_finish(attributes),
            "id" => Ok(attributes
                .get("id")
                .or_else(|| attributes.get("thread_id"))
                .cloned()
                .unwrap_or(Value::Int(0))),
            "WHAT" => Ok(Value::Package(crate::symbol::Symbol::intern("Thread"))),
            "Str" | "gist" => Ok(Value::str_from("Thread")),
            _ => Err(RuntimeError::new(format!(
                "No method '{}' on Thread",
                method
            ))),
        }
    }
}
