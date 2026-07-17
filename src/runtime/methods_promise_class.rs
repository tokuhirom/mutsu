use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// Promise.start / Thread.start dispatch
    pub(super) fn dispatch_promise_start(
        &mut self,
        target: &Value,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if let ValueView::Instance { class_name, .. } = target.view()
            && class_name == "Supply"
        {
            return Some(self.dispatch_supply_transform(target.clone(), "start", args));
        }
        if let Some(cls) = self.promise_class_name(target) {
            let block = args.first().cloned().unwrap_or(Value::NIL);
            return Some(Ok(self.spawn_callable_promise(block, Symbol::intern(&cls))));
        }
        // Thread.start
        if let ValueView::Package(class_name) = target.view()
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
            let ret = Value::promise(promise.clone());
            // Registered spawn: this thread drops a `Gc` promise handle at
            // exit (see `spawn_gc_helper_thread`); the sleep is a quiescent
            // safe region so a long timer never starves a stop-the-world.
            crate::runtime::builtins_system::spawn_gc_helper_thread(move || {
                if secs > 0.0 {
                    crate::gc::block_quiescent(|| {
                        std::thread::sleep(Duration::from_secs_f64(secs))
                    });
                }
                promise.keep(Value::TRUE, String::new(), String::new());
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
            let at_time = match args.first().map(|v| (v, v.view())) {
                Some((
                    _,
                    ValueView::Instance {
                        class_name,
                        attributes,
                        ..
                    },
                )) if class_name == "Instant" => {
                    let tai = attributes
                        .as_map()
                        .get("value")
                        .map(|v| v.to_f64())
                        .unwrap_or(0.0);
                    crate::builtins::methods_0arg::temporal::instant_to_posix(tai)
                }
                Some((v, _)) => v.to_f64(),
                None => 0.0,
            };
            let now = crate::value::current_time_secs_f64();
            let delay = (at_time - now).max(0.0);
            let promise = SharedPromise::new_with_class(Symbol::intern(&cls));
            let ret = Value::promise(promise.clone());
            // Registered spawn + quiescent sleep — see `dispatch_promise_in`.
            crate::runtime::builtins_system::spawn_gc_helper_thread(move || {
                if delay > 0.0 {
                    crate::gc::block_quiescent(|| {
                        std::thread::sleep(Duration::from_secs_f64(delay))
                    });
                }
                promise.keep(Value::TRUE, String::new(), String::new());
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
            let value = args.first().cloned().unwrap_or(Value::TRUE);
            let promise = SharedPromise::new_with_class(Symbol::intern(&cls));
            promise.keep(value, String::new(), String::new());
            return Some(Ok(Value::promise(promise)));
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
            return Some(Ok(Value::promise(promise)));
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
            let ret = Value::promise(promise.clone());
            let promises = match self.collect_promise_combinator_inputs("allof", args) {
                Ok(p) => p,
                Err(e) => return Some(Err(e)),
            };
            super::native_methods::register_promise_combinator_sources(&promise, promises.clone());
            if promises.is_empty() {
                promise.keep(Value::TRUE, String::new(), String::new());
                return Some(Ok(ret));
            }
            // Registered spawn: `p.wait()` clones arbitrary result `Value`s
            // and the thread drops its promise handles at exit (the waits
            // themselves are already STW-aware / quiescent).
            crate::runtime::builtins_system::spawn_gc_helper_thread(move || {
                for p in &promises {
                    p.wait();
                }
                promise.keep(Value::TRUE, String::new(), String::new());
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
            let ret = Value::promise(promise.clone());
            let promises = match self.collect_promise_combinator_inputs("anyof", args) {
                Ok(p) => p,
                Err(e) => return Some(Err(e)),
            };
            if promises.is_empty() {
                promise.keep(Value::TRUE, String::new(), String::new());
                return Some(Ok(ret));
            }
            // Resolve on the first input to settle, via each input's
            // `on_resolve` waiter queue — no polling thread. `try_keep` makes
            // the first resolver win and later ones no-ops.
            for p in &promises {
                let anyof = promise.clone();
                let _ = p.on_resolve(Box::new(move |_, _, _, _| {
                    let _ = anyof.try_keep(Value::TRUE);
                }));
            }
            return Some(Ok(ret));
        }
        None
    }
}
