//! ADR-0105 D1: a promise carries the scheduler it was constructed under.
//!
//! Rakudo binds `Promise.new(:$scheduler = $*SCHEDULER)` at every promise it
//! makes (`.in`/`.at`, `start`, `.then` results, `allof`/`anyof`, ...), reads
//! it back through `.scheduler`, and cues a `start` body through a user
//! `$*SCHEDULER`. mutsu keeps only *user* schedulers on the promise; a
//! built-in one stays `None` and keeps the native deadline-heap / worker-pool
//! paths, which are observationally identical and much cheaper.

use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// The `$*SCHEDULER` currently in effect, but only when it is a
    /// *user-defined* scheduler — a class that is not one of the built-ins.
    ///
    /// Raku defines `Promise.in($t)` as `$*SCHEDULER.cue({ ... }, :in($t))`, so
    /// swapping `$*SCHEDULER` (e.g. for `Test::Scheduler`'s virtual time)
    /// redirects every timed promise. mutsu drives the built-in schedulers
    /// straight off the shared deadline heap, which is much cheaper and
    /// observationally identical, so only a user scheduler needs the real
    /// `.cue` dispatch.
    pub(in crate::runtime) fn user_scheduler(&mut self) -> Option<Value> {
        let sched = self.env().get("*SCHEDULER")?.clone();
        let ValueView::Instance { class_name, .. } = sched.view() else {
            return None;
        };
        if Self::is_builtin_scheduler(&sched) {
            return None;
        }
        // Only a scheduler that actually provides `cue` can drive the promise.
        self.class_has_method(&class_name.resolve(), "cue")
            .then_some(sched)
    }

    /// Whether `value` is one of the built-in schedulers (instance or type
    /// object), which mutsu drives natively instead of through `.cue`.
    fn is_builtin_scheduler(value: &Value) -> bool {
        let name = match value.view() {
            ValueView::Instance { class_name, .. } => class_name,
            ValueView::Package(name) => name,
            _ => return false,
        };
        name == "Scheduler"
            || name == "ThreadPoolScheduler"
            || name == "CurrentThreadScheduler"
            || name == "FakeScheduler"
    }

    /// The scheduler a new promise binds (ADR-0105 D1), as Rakudo's
    /// `Promise.new(:$scheduler = $*SCHEDULER)` does: an explicit
    /// `:scheduler` wins, else the dynamic `$*SCHEDULER`. Only a *user*
    /// scheduler is kept; a built-in one binds as `None` (mutsu's native
    /// paths). An explicit non-scheduler value (`Promise.new(scheduler =>
    /// 42)`) is kept as given, since `.scheduler` must hand it back.
    pub(in crate::runtime) fn promise_scheduler_binding(
        &mut self,
        explicit: Option<Value>,
    ) -> Option<Value> {
        match explicit {
            Some(sched) if sched.is_nil() || Self::is_builtin_scheduler(&sched) => None,
            Some(sched) => Some(sched),
            None => self.user_scheduler(),
        }
    }

    /// A new planned promise of class `class_name`, bound to the scheduler
    /// `Promise.new` would bind (see [`Self::promise_scheduler_binding`]).
    pub(in crate::runtime) fn new_bound_promise(
        &mut self,
        class_name: Symbol,
        explicit_scheduler: Option<Value>,
    ) -> SharedPromise {
        let promise = SharedPromise::new_with_class(class_name);
        promise.set_scheduler(self.promise_scheduler_binding(explicit_scheduler));
        promise
    }

    /// `Promise.scheduler`: the bound user scheduler, else the built-in one
    /// (the process `$*SCHEDULER`, which is what an unbound promise was
    /// constructed under).
    pub(in crate::runtime) fn promise_scheduler_value(&mut self, shared: &SharedPromise) -> Value {
        if let Some(sched) = shared.scheduler() {
            return sched;
        }
        match self.env().get("*SCHEDULER") {
            Some(sched) if Self::is_builtin_scheduler(sched) => sched.clone(),
            _ => Value::make_instance(
                Symbol::intern("ThreadPoolScheduler"),
                std::collections::HashMap::new(),
            ),
        }
    }

    /// A zero-arg bare block whose body is `body`, as a first-class
    /// `Callable` a user scheduler can store and invoke later. The body is a
    /// single call into a native method on a literal instance, so the block
    /// itself does no work of its own.
    pub(in crate::runtime) fn synthesized_thunk(body: Vec<crate::ast::Stmt>) -> Value {
        Value::sub_value(crate::gc::Gc::new(crate::value::SubData {
            package: Symbol::intern("GLOBAL"),
            name: Symbol::intern(""),
            params: crate::value::empty_params(),
            param_defs: crate::value::empty_param_defs(),
            body: std::sync::Arc::new(body),
            is_rw: false,
            is_raw: false,
            env: Env::new(),
            assumed_positional: Vec::new(),
            assumed_named: ValueMap::default(),
            id: crate::value::next_instance_id(),
            empty_sig: false,
            is_bare_block: true,
            compiled_code: None,
            compiled_fns: None,
            compiled_routine: None,
            is_decl_expr_thunk: false,
            deprecated_message: None,
            source_line: None,
            source_file: None,
            owned_captures: Vec::new(),
            authoritative_captures: Vec::new(),
            upvalues: Vec::new(),
            captured_fatal_mode: false,
            param_name_syms_cache: std::sync::OnceLock::new(),
            source_file_sym_cache: std::sync::OnceLock::new(),
            state_scope_guard: None,
        }))
    }

    /// `start { ... }` / `Promise.start`: under a bound user scheduler the
    /// body is cued through that scheduler's `.cue`, as Rakudo does
    /// (`Test::Time`'s `:auto-advance` escapes a virtual-time scheduler with
    /// an explicit `:scheduler($orig)` for exactly this reason); otherwise it
    /// runs on the worker pool.
    // Cost: O(1) on the pool path; O(cue) for a user scheduler, cue = the
    // user scheduler's `.cue` method.
    pub(in crate::runtime) fn start_callable_promise(
        &mut self,
        block: Value,
        class_name: Symbol,
        scheduler: Option<Value>,
    ) -> Result<Value, RuntimeError> {
        let Some(scheduler) = scheduler else {
            return Ok(self.spawn_callable_promise(block, class_name));
        };
        let promise = SharedPromise::new_with_class(class_name);
        promise.set_scheduler(Some(scheduler.clone()));
        // The body's outcome resolves the promise, so the runtime holds its
        // vow — the same as the pool path.
        promise.mark_vowed();
        let block = Self::strip_start_block_topics(block);
        let thunk = Self::promise_start_thunk(&promise, block);
        // TODO: Rakudo also passes `:catch(-> $ex { $vow.break($ex) })`; the
        // thunk breaks the promise itself instead, because a synthesized
        // block has no parameter list to receive the exception with.
        self.call_method_with_values(scheduler, "cue", vec![thunk])?;
        Ok(Value::promise(promise))
    }

    /// Raku gives each `start` block fresh `$/` and `$!`: strip them from the
    /// closure's captured env so they don't override the thread's fresh Nil
    /// values.
    pub(in crate::runtime) fn strip_start_block_topics(block: Value) -> Value {
        let ValueView::Sub(data) = block.view() else {
            return block;
        };
        let mut new_data = (**data).clone();
        new_data.env.remove("/");
        new_data.env.remove("!");
        new_data.env.remove("$/");
        new_data.env.remove("$!");
        Value::sub_value(crate::gc::Gc::new(new_data))
    }

    /// The block a user scheduler is cued with for `start`: its body is
    /// `$vow.__mutsu_run_start(&block)`, which runs `block` and keeps or
    /// breaks the promise with its outcome.
    fn promise_start_thunk(promise: &SharedPromise, block: Value) -> Value {
        let mut vow_attrs = std::collections::HashMap::new();
        vow_attrs.insert("promise".to_string(), Value::promise(promise.clone()));
        let vow = Value::make_instance(Symbol::intern("Promise::Vow"), vow_attrs);
        let body = vec![crate::ast::Stmt::Expr(crate::ast::Expr::MethodCall {
            target: Box::new(crate::ast::Expr::Literal(vow)),
            name: Symbol::intern("__mutsu_run_start"),
            args: vec![crate::ast::Expr::Literal(block)],
            modifier: None,
            quoted: false,
        })];
        Self::synthesized_thunk(body)
    }

    /// `Promise::Vow.__mutsu_run_start(&block)`: run a scheduler-cued `start`
    /// body on the current thread and resolve the vowed promise with its
    /// outcome (Rakudo's `$vow.keep(code())` / `:catch` pair).
    // Cost: O(1) plus the block's own cost.
    pub(in crate::runtime) fn run_cued_start_body(
        &mut self,
        promise: &SharedPromise,
        block: Value,
    ) -> Result<Value, RuntimeError> {
        promise.set_thread_id(crate::runtime::current_mutsu_thread_id());
        let result = crate::vm::guard_worker_panic(|| self.call_value(block, vec![]));
        match result {
            Ok(v) => {
                let _ = promise.try_keep(v);
            }
            Err(e) => {
                let error_val = e
                    .exception
                    .map(|ex| *ex)
                    .unwrap_or_else(|| Value::str(e.message.into_owned()));
                let _ = promise.try_break(error_val);
            }
        }
        Ok(Value::NIL)
    }
}
