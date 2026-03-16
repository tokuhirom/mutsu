use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    fn as_exception_value(value: Value) -> Value {
        match value {
            Value::Instance { class_name, .. }
                if class_name.resolve().contains("Exception")
                    || class_name.resolve().starts_with("X::") =>
            {
                value
            }
            other => {
                let msg = other.to_string_value();
                let mut attrs = HashMap::new();
                attrs.insert("payload".to_string(), Value::str(msg.clone()));
                attrs.insert("message".to_string(), Value::str(msg));
                Value::make_instance(Symbol::intern("X::AdHoc"), attrs)
            }
        }
    }

    fn channel_send_closed_error() -> RuntimeError {
        let mut err = RuntimeError::new("Cannot send on a closed channel");
        err.exception = Some(Box::new(Value::make_instance(
            Symbol::intern("X::Channel::SendOnClosed"),
            HashMap::new(),
        )));
        err
    }

    fn channel_receive_closed_error() -> RuntimeError {
        let mut err = RuntimeError::new("Cannot receive on a closed channel");
        err.exception = Some(Box::new(Value::make_instance(
            Symbol::intern("X::Channel::ReceiveOnClosed"),
            HashMap::new(),
        )));
        err
    }

    pub(super) fn dispatch_promise_method(
        &mut self,
        shared: &SharedPromise,
        method: &str,
        args: Vec<Value>,
        target: &Value,
    ) -> Result<Value, RuntimeError> {
        match method {
            "result" => {
                let status = shared.status();
                if status == "Broken" {
                    // .result on a Broken promise throws the cause as X::AdHoc
                    let (result, _, _) = shared.wait();
                    self.sync_shared_vars_to_env();
                    let msg = result.to_string_value();
                    let mut attrs = HashMap::new();
                    attrs.insert("payload".to_string(), Value::str(msg.clone()));
                    attrs.insert("message".to_string(), Value::str(msg.clone()));
                    let ex = Value::make_instance(Symbol::intern("X::AdHoc"), attrs);
                    let mut err = RuntimeError::new(msg);
                    err.exception = Some(Box::new(ex));
                    Err(err)
                } else {
                    // Planned blocks, Kept returns value
                    let result = shared.result_blocking();
                    self.sync_shared_vars_to_env();
                    // Replay deferred taps for Proc::Async results
                    if let Value::Instance {
                        ref class_name,
                        ref attributes,
                        ..
                    } = result
                        && class_name == "Proc"
                    {
                        self.replay_proc_taps(attributes);
                    }
                    Ok(result)
                }
            }
            "status" => Ok(Value::str(shared.status())),
            "then" => {
                let block = args.into_iter().next().unwrap_or(Value::Nil);
                let orig = shared.clone();
                let new_promise = SharedPromise::new_with_class(shared.class_name());
                let ret = Value::Promise(new_promise.clone());
                if orig.is_resolved() {
                    let (result, output, stderr) = orig.wait();
                    match self.call_sub_value(block, vec![result], true) {
                        Ok(v) => new_promise.keep(v, output, stderr),
                        Err(e) => {
                            let error_val = if let Some(ex) = e.exception {
                                *ex
                            } else {
                                Value::str(e.message)
                            };
                            new_promise.break_with(error_val, output, stderr);
                        }
                    }
                } else {
                    let mut thread_interp = self.clone_for_thread();
                    std::thread::spawn(move || {
                        let (result, output, stderr) = orig.wait();
                        match thread_interp.call_sub_value(block, vec![result], true) {
                            Ok(v) => {
                                let out = std::mem::take(&mut thread_interp.output);
                                let err = std::mem::take(&mut thread_interp.stderr_output);
                                new_promise.keep(
                                    v,
                                    format!("{}{}", output, out),
                                    format!("{}{}", stderr, err),
                                );
                            }
                            Err(e) => {
                                let out = std::mem::take(&mut thread_interp.output);
                                let err = std::mem::take(&mut thread_interp.stderr_output);
                                let error_val = if let Some(ex) = e.exception {
                                    *ex
                                } else {
                                    Value::str(e.message)
                                };
                                new_promise.break_with(
                                    error_val,
                                    format!("{}{}", output, out),
                                    format!("{}{}", stderr, err),
                                );
                            }
                        }
                    });
                }
                Ok(ret)
            }
            "keep" => {
                let value = args.into_iter().next().unwrap_or(Value::Bool(true));
                if let Err(_status) = shared.try_keep(value) {
                    let mut attrs = HashMap::new();
                    attrs.insert(
                        "message".to_string(),
                        Value::str(
                            "Access denied to keep/break this Promise; already vowed".to_string(),
                        ),
                    );
                    let ex = Value::make_instance(Symbol::intern("X::Promise::Vowed"), attrs);
                    let mut err = RuntimeError::new(
                        "Access denied to keep/break this Promise; already vowed".to_string(),
                    );
                    err.exception = Some(Box::new(ex));
                    Err(err)
                } else {
                    Ok(Value::Nil)
                }
            }
            "break" => {
                let reason_val = args
                    .into_iter()
                    .next()
                    .unwrap_or_else(|| Value::str_from("Died"));
                if let Err(_status) = shared.try_break(reason_val) {
                    let mut attrs = HashMap::new();
                    attrs.insert(
                        "message".to_string(),
                        Value::str(
                            "Access denied to keep/break this Promise; already vowed".to_string(),
                        ),
                    );
                    let ex = Value::make_instance(Symbol::intern("X::Promise::Vowed"), attrs);
                    let mut err = RuntimeError::new(
                        "Access denied to keep/break this Promise; already vowed".to_string(),
                    );
                    err.exception = Some(Box::new(ex));
                    Err(err)
                } else {
                    Ok(Value::Nil)
                }
            }
            "cause" => {
                let status = shared.status();
                if status != "Broken" {
                    let mut attrs = HashMap::new();
                    attrs.insert("status".to_string(), Value::str(status.clone()));
                    attrs.insert(
                        "message".to_string(),
                        Value::str(format!(
                            "Can only call '.cause' on a broken promise (status: {})",
                            status
                        )),
                    );
                    let ex = Value::make_instance(
                        Symbol::intern("X::Promise::CauseOnlyValidOnBroken"),
                        attrs,
                    );
                    let mut err = RuntimeError::new(format!(
                        "Can only call '.cause' on a broken promise (status: {})",
                        status
                    ));
                    err.exception = Some(Box::new(ex));
                    Err(err)
                } else {
                    // Broken
                    let (result, _, _) = shared.wait();
                    // Wrap in X::AdHoc if it's a plain string
                    let cause = match &result {
                        Value::Instance { class_name, .. }
                            if class_name.resolve().contains("Exception")
                                || class_name.resolve().starts_with("X::") =>
                        {
                            result
                        }
                        _ => {
                            let mut attrs = HashMap::new();
                            attrs.insert(
                                "payload".to_string(),
                                Value::str(result.to_string_value()),
                            );
                            attrs.insert(
                                "message".to_string(),
                                Value::str(result.to_string_value()),
                            );
                            Value::make_instance(Symbol::intern("X::AdHoc"), attrs)
                        }
                    };
                    Ok(cause)
                }
            }
            "Bool" => Ok(Value::Bool(shared.is_resolved())),
            "vow" => {
                // Return a simple Vow object
                let mut attrs = HashMap::new();
                attrs.insert("promise".to_string(), target.clone());
                Ok(Value::make_instance(Symbol::intern("Promise::Vow"), attrs))
            }
            "WHAT" => Ok(Value::Package(shared.class_name())),
            "raku" | "perl" => Ok(Value::str(format!(
                "Promise.new(status => {})",
                shared.status()
            ))),
            "Str" | "gist" => Ok(Value::str(format!("Promise({})", shared.status()))),
            "isa" => {
                let target_name = match args.first().cloned().unwrap_or(Value::Nil) {
                    Value::Package(name) => name.resolve(),
                    Value::Str(name) => name.to_string(),
                    other => other.to_string_value(),
                };
                let cn = shared.class_name().resolve();
                let is_match = cn == target_name
                    || target_name == "Promise"
                    || target_name == "Any"
                    || target_name == "Mu"
                    || self.class_mro(&cn).contains(&target_name);
                Ok(Value::Bool(is_match))
            }
            _ => Err(RuntimeError::new(format!(
                "No method '{}' on Promise",
                method
            ))),
        }
    }

    pub(super) fn dispatch_promise_vow_method(
        &mut self,
        attributes: &HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let promise = attributes
            .get("promise")
            .cloned()
            .ok_or_else(|| RuntimeError::new("Promise::Vow has no backing promise"))?;
        let Value::Promise(shared) = promise else {
            return Err(RuntimeError::new(
                "Promise::Vow backing value is not a Promise",
            ));
        };
        match method {
            "keep" | "break" => {
                let target = Value::Promise(shared.clone());
                self.dispatch_promise_method(&shared, method, args, &target)
            }
            "WHAT" => Ok(Value::Package(Symbol::intern("Promise::Vow"))),
            "Str" | "gist" => Ok(Value::str("(Vow)".to_string())),
            _ => Err(RuntimeError::new(format!(
                "No method '{}' on Promise::Vow",
                method
            ))),
        }
    }

    pub(super) fn dispatch_channel_method(
        &mut self,
        ch: &SharedChannel,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        match method {
            "send" => {
                if !ch.can_send() {
                    return Err(Self::channel_send_closed_error());
                }
                let value = args.into_iter().next().unwrap_or(Value::Nil);
                ch.send(value);
                Ok(Value::Nil)
            }
            "receive" => match ch.receive_result() {
                Ok(Value::Nil) => Err(Self::channel_receive_closed_error()),
                Ok(value) => Ok(value),
                Err(cause) => {
                    let ex = Self::as_exception_value(cause);
                    let mut err = RuntimeError::new(ex.to_string_value());
                    err.exception = Some(Box::new(ex));
                    Err(err)
                }
            },
            "poll" => match ch.poll_result() {
                Ok(Some(value)) => Ok(value),
                Ok(None) => Ok(Value::Nil),
                Err(_) => Ok(Value::Nil),
            },
            "close" => {
                ch.close();
                Ok(Value::Nil)
            }
            "fail" => {
                let reason = args
                    .into_iter()
                    .next()
                    .unwrap_or_else(|| Value::str_from("Died"));
                ch.fail(Self::as_exception_value(reason));
                Ok(Value::Nil)
            }
            "list" | "List" | "Array" | "Seq" => {
                // Drain the channel into a list (blocks until closed)
                let mut items = Vec::new();
                loop {
                    match ch.receive_result() {
                        Ok(Value::Nil) => break,
                        Ok(value) => items.push(value),
                        Err(cause) => {
                            let ex = Self::as_exception_value(cause);
                            let mut err = RuntimeError::new(ex.to_string_value());
                            err.exception = Some(Box::new(ex));
                            return Err(err);
                        }
                    }
                }
                Ok(Value::array(items))
            }
            "closed" => Ok(Value::Promise(ch.closed_promise())),
            "elems" => Err(RuntimeError::new(
                "Cannot call '.elems' on a Channel instance".to_string(),
            )),
            "Bool" => Ok(Value::Bool(true)),
            "WHAT" => Ok(Value::Package(Symbol::intern("Channel"))),
            "Str" | "gist" => Ok(Value::str_from("Channel")),
            _ => Err(RuntimeError::new(format!(
                "No method '{}' on Channel",
                method
            ))),
        }
    }
}
