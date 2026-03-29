use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// Promise.start / Thread.start dispatch
    pub(super) fn dispatch_promise_start(
        &mut self,
        target: &Value,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if let Value::Instance { class_name, .. } = target
            && class_name == "Supply"
        {
            return Some(self.dispatch_supply_transform(target.clone(), "start", args));
        }
        if let Some(cls) = self.promise_class_name(target) {
            let block = args.first().cloned().unwrap_or(Value::Nil);
            return Some(Ok(self.spawn_callable_promise(block, Symbol::intern(&cls))));
        }
        // Thread.start
        if let Value::Package(class_name) = target
            && class_name == "Thread"
        {
            return Some(self.dispatch_thread_start(args));
        }
        None
    }

    /// Promise.in dispatch
    pub(super) fn dispatch_promise_in(
        &mut self,
        target: &Value,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if let Some(cls) = self.promise_class_name(target) {
            let secs = args.first().map(|v| v.to_f64()).unwrap_or(0.0).max(0.0);
            let promise = SharedPromise::new_with_class(Symbol::intern(&cls));
            let ret = Value::Promise(promise.clone());
            std::thread::spawn(move || {
                if secs > 0.0 {
                    std::thread::sleep(Duration::from_secs_f64(secs));
                }
                promise.keep(Value::Bool(true), String::new(), String::new());
            });
            return Some(Ok(ret));
        }
        None
    }

    /// Promise.at dispatch
    pub(super) fn dispatch_promise_at(
        &mut self,
        target: &Value,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if let Some(cls) = self.promise_class_name(target) {
            // at_time may be an Instant (TAI) or a plain numeric (POSIX).
            // Convert Instant values to POSIX for delay calculation.
            let at_time = match args.first() {
                Some(Value::Instance {
                    class_name,
                    attributes,
                    ..
                }) if class_name == "Instant" => {
                    let tai = attributes.get("value").map(|v| v.to_f64()).unwrap_or(0.0);
                    crate::builtins::methods_0arg::temporal::instant_to_posix(tai)
                }
                Some(v) => v.to_f64(),
                None => 0.0,
            };
            let now = crate::value::current_time_secs_f64();
            let delay = (at_time - now).max(0.0);
            let promise = SharedPromise::new_with_class(Symbol::intern(&cls));
            let ret = Value::Promise(promise.clone());
            std::thread::spawn(move || {
                if delay > 0.0 {
                    std::thread::sleep(Duration::from_secs_f64(delay));
                }
                promise.keep(Value::Bool(true), String::new(), String::new());
            });
            return Some(Ok(ret));
        }
        None
    }

    /// Promise.kept dispatch
    pub(super) fn dispatch_promise_kept(
        &mut self,
        target: &Value,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if let Some(cls) = self.promise_class_name(target) {
            let value = args.first().cloned().unwrap_or(Value::Bool(true));
            let promise = SharedPromise::new_with_class(Symbol::intern(&cls));
            promise.keep(value, String::new(), String::new());
            return Some(Ok(Value::Promise(promise)));
        }
        None
    }

    /// Promise.broken dispatch
    pub(super) fn dispatch_promise_broken(
        &mut self,
        target: &Value,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if let Some(cls) = self.promise_class_name(target) {
            let reason_val = args
                .first()
                .cloned()
                .unwrap_or_else(|| Value::str_from("Died"));
            let promise = SharedPromise::new_with_class(Symbol::intern(&cls));
            promise.break_with(reason_val, String::new(), String::new());
            return Some(Ok(Value::Promise(promise)));
        }
        None
    }

    /// Promise.allof dispatch
    pub(super) fn dispatch_promise_allof(
        &mut self,
        target: &Value,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if let Some(cls) = self.promise_class_name(target) {
            let promise = SharedPromise::new_with_class(Symbol::intern(&cls));
            let ret = Value::Promise(promise.clone());
            let promises = match self.collect_promise_combinator_inputs("allof", args) {
                Ok(p) => p,
                Err(e) => return Some(Err(e)),
            };
            super::native_methods::register_promise_combinator_sources(&promise, promises.clone());
            if promises.is_empty() {
                promise.keep(Value::Bool(true), String::new(), String::new());
                return Some(Ok(ret));
            }
            std::thread::spawn(move || {
                for p in &promises {
                    p.wait();
                }
                promise.keep(Value::Bool(true), String::new(), String::new());
            });
            return Some(Ok(ret));
        }
        None
    }

    /// Promise.anyof dispatch
    pub(super) fn dispatch_promise_anyof(
        &mut self,
        target: &Value,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if let Some(cls) = self.promise_class_name(target) {
            let promise = SharedPromise::new_with_class(Symbol::intern(&cls));
            let ret = Value::Promise(promise.clone());
            let promises = match self.collect_promise_combinator_inputs("anyof", args) {
                Ok(p) => p,
                Err(e) => return Some(Err(e)),
            };
            if promises.is_empty() {
                promise.keep(Value::Bool(true), String::new(), String::new());
                return Some(Ok(ret));
            }
            std::thread::spawn(move || {
                // Poll until any promise resolves
                loop {
                    for p in &promises {
                        if p.status() != "Planned" {
                            promise.keep(Value::Bool(true), String::new(), String::new());
                            return;
                        }
                    }
                    std::thread::sleep(Duration::from_millis(1));
                }
            });
            return Some(Ok(ret));
        }
        None
    }
}
