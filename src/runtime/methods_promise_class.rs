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

    /// Keep `promise` with True once `secs` have elapsed, via the shared
    /// deadline-heap timer — no per-timer OS thread. `keep` never runs user
    /// code on the resolver (waiters dispatch to their own thread), so it is
    /// safe on the timer driver. `+Inf` means the promise never resolves.
    fn keep_promise_after(promise: SharedPromise, secs: f64) {
        if secs == f64::INFINITY {
            return;
        }
        super::native_methods::interval_timer::register_once(
            super::native_methods::interval_timer::clamp_delay_secs(secs),
            Box::new(move || {
                promise.keep(Value::TRUE, String::new(), String::new());
            }),
        );
    }

    /// The `$*SCHEDULER` currently in effect, but only when it is a
    /// *user-defined* scheduler — a class that is not one of the built-ins.
    ///
    /// Raku defines `Promise.in($t)` as `$*SCHEDULER.cue({ ... }, :in($t))`, so
    /// swapping `$*SCHEDULER` (e.g. for `Test::Scheduler`'s virtual time)
    /// redirects every timed promise. mutsu drives the built-in schedulers
    /// straight off the shared deadline heap, which is much cheaper and
    /// observationally identical, so only a user scheduler needs the real
    /// `.cue` dispatch.
    fn user_scheduler(&mut self) -> Option<Value> {
        let sched = self.env().get("*SCHEDULER")?.clone();
        let ValueView::Instance { class_name, .. } = sched.view() else {
            return None;
        };
        let name = class_name.resolve();
        if matches!(
            name.as_str(),
            "Scheduler" | "ThreadPoolScheduler" | "CurrentThreadScheduler" | "FakeScheduler"
        ) {
            return None;
        }
        // Only a scheduler that actually provides `cue` can drive the promise.
        self.class_has_method(&name, "cue").then_some(sched)
    }

    /// Hand `promise` to a user `$*SCHEDULER` via `.cue(&keeper, :$in)`, where
    /// `&keeper` is a synthesized zero-arg block whose body is
    /// `$promise.keep(True)`. Returns the scheduler's cancellation value, which
    /// the caller discards (`Promise.in` returns the promise itself).
    fn cue_promise_on_scheduler(
        &mut self,
        scheduler: Value,
        promise: &SharedPromise,
        delay_key: &str,
        delay: f64,
    ) -> Result<Value, RuntimeError> {
        let keeper = Self::promise_keeper_block(promise);
        let named = Value::pair(delay_key.to_string(), Value::num(delay));
        self.call_method_with_values(scheduler, "cue", vec![keeper, named])
    }

    /// A zero-arg block that keeps `promise` with `True`, as a first-class
    /// `Callable` a user scheduler can store and invoke later.
    fn promise_keeper_block(promise: &SharedPromise) -> Value {
        let body = vec![crate::ast::Stmt::Expr(crate::ast::Expr::MethodCall {
            target: Box::new(crate::ast::Expr::Literal(Value::promise(promise.clone()))),
            name: Symbol::intern("keep"),
            args: vec![crate::ast::Expr::Literal(Value::TRUE)],
            modifier: None,
            quoted: false,
        })];
        Value::sub_value(crate::gc::Gc::new(crate::value::SubData {
            package: Symbol::intern("GLOBAL"),
            name: Symbol::intern(""),
            params: Vec::new(),
            param_defs: Vec::new(),
            body,
            is_rw: false,
            is_raw: false,
            env: Env::new(),
            assumed_positional: Vec::new(),
            assumed_named: std::collections::HashMap::new(),
            id: crate::value::next_instance_id(),
            empty_sig: false,
            is_bare_block: true,
            compiled_code: None,
            deprecated_message: None,
            source_line: None,
            source_file: None,
            owned_captures: Vec::new(),
            authoritative_captures: Vec::new(),
            upvalues: Vec::new(),
        }))
    }

    /// Promise.in dispatch
    pub(super) fn dispatch_promise_in(
        &mut self,
        target: &Value,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if let Some(cls) = self.promise_class_name(target) {
            let secs = args.first().map(|v| v.to_f64()).unwrap_or(0.0);
            let promise = SharedPromise::new_with_class(Symbol::intern(&cls));
            let ret = Value::promise(promise.clone());
            if let Some(scheduler) = self.user_scheduler() {
                if let Err(e) = self.cue_promise_on_scheduler(scheduler, &promise, "in", secs) {
                    return Some(Err(e));
                }
                return Some(Ok(ret));
            }
            Self::keep_promise_after(promise, secs);
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
            let delay = at_time - now;
            let promise = SharedPromise::new_with_class(Symbol::intern(&cls));
            let ret = Value::promise(promise.clone());
            if let Some(scheduler) = self.user_scheduler() {
                // Rakudo's `Promise.at` cues with `:in($at - now)`, not `:at`,
                // so a virtual-time scheduler measures the delay from its own
                // clock. Match that.
                if let Err(e) = self.cue_promise_on_scheduler(scheduler, &promise, "in", delay) {
                    return Some(Err(e));
                }
                return Some(Ok(ret));
            }
            Self::keep_promise_after(promise, delay);
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
            super::native_methods::register_promise_combinator_sources(
                &promise,
                super::native_methods::PromiseCombinator::Allof,
                promises.clone(),
            );
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
            // Record the sources so that awaiting the composite can still replay
            // the deferred `Proc::Async` taps of whichever source has settled —
            // an `anyof` result is a plain `True`, so the `Proc`-result hook in
            // `await` would otherwise never fire.
            super::native_methods::register_promise_combinator_sources(
                &promise,
                super::native_methods::PromiseCombinator::Anyof,
                promises.clone(),
            );
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
