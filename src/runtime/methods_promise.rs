use super::*;
use crate::symbol::Symbol;
use crate::value::AttrMap;

impl Interpreter {
    pub(crate) fn as_exception_value(value: Value) -> Value {
        if matches!(
            value.view(),
            ValueView::Instance { class_name, .. }
                if class_name.as_str().contains("Exception")
                    || class_name.as_str().starts_with("X::")
        ) {
            return value;
        }
        let msg = value.to_string_value();
        let mut attrs = HashMap::new();
        attrs.insert("payload".to_string(), Value::str(msg.clone()));
        attrs.insert("message".to_string(), Value::str(msg));
        Value::make_instance(Symbol::intern("X::AdHoc"), attrs)
    }

    fn channel_send_closed_error() -> RuntimeError {
        let mut err = RuntimeError::new("Cannot send on a closed channel");
        err.exception = Some(Box::new(Value::make_instance(
            Symbol::intern("X::Channel::SendOnClosed"),
            HashMap::new(),
        )));
        err
    }

    pub(crate) fn channel_receive_closed_error() -> RuntimeError {
        let mut err = RuntimeError::new("Cannot receive on a closed channel");
        err.exception = Some(Box::new(Value::make_instance(
            Symbol::intern("X::Channel::ReceiveOnClosed"),
            HashMap::new(),
        )));
        err
    }

    /// Resolve a callback result into the new promise, keeping on success,
    /// breaking on error.
    ///
    /// The derived promise inherits the original's scheduler, so a user
    /// scheduler dispatches its subscribers (ADR-0105 D2).
    fn resolve_promise_callback(
        &mut self,
        new_promise: &SharedPromise,
        result: Result<Value, RuntimeError>,
        output: String,
        stderr: String,
    ) {
        let (kept, value) = match result {
            Ok(v) => (true, v),
            Err(e) => {
                let error_val = if let Some(ex) = e.exception {
                    *ex
                } else {
                    Value::str(e.message.into_owned())
                };
                (false, error_val)
            }
        };
        // A scheduler whose `.cue` fails has already had the subscribers
        // dispatched the built-in way; there is no caller to report it to.
        let _ = self.resolve_promise_dispatching(new_promise, kept, value, Some((output, stderr)));
    }

    /// Run a promise chaining method (.then, .andthen, .orelse).
    /// `should_run` decides whether to invoke the callback based on the
    /// resolved status; returns `None` to skip (propagate), `Some(true)` to
    /// run the callback.  `propagate` produces the value to forward when
    /// skipping. Invoke callbacks through the VM callable path so their
    /// compiled lexical `&name` calls keep the same resolution as direct calls.
    // Cost: O(1) excluding the callback, which runs user code.
    fn promise_chain_method(
        &mut self,
        shared: &SharedPromise,
        block: Value,
        should_run: fn(&str) -> bool,
        propagate_kept: bool,
    ) -> Value {
        let orig = shared.clone();
        let new_promise = SharedPromise::new_with_class(shared.class_name());
        // Rakudo's `.then` result is `self.new(:$!scheduler)`: it inherits the
        // original's scheduler (ADR-0105 D1).
        new_promise.set_scheduler(shared.scheduler());
        // The callback's outcome resolves the derived promise, so mutsu holds
        // its vow: `$p.then({...}).keep` is X::Promise::Vowed in Rakudo.
        new_promise.mark_vowed();
        let ret = Value::promise(new_promise.clone());
        if orig.is_resolved() {
            let (result, output, stderr) = orig.wait();
            let status = orig.status();
            if should_run(&status) {
                let promise_val = Value::promise(orig);
                // The callback's failure breaks the derived promise; it never
                // reaches the caller. Behind a catch marker, so a `CATCH` around
                // the `.then` call is not run inline at the callback's `die`
                // (ADR-0072) — it would run again when the promise is awaited.
                // Run inline, but as a stack of its own (`$*STACK-ID`), as
                // Rakudo's scheduled callback is.
                let cb_result = crate::runtime::stack_id::run_on_fresh_stack(|| {
                    self.with_catch_marker(|this| {
                        this.vm_call_on_value(block, vec![promise_val], None)
                    })
                });
                self.resolve_promise_callback(&new_promise, cb_result, output, stderr);
            } else if propagate_kept {
                new_promise.keep(result, output, stderr);
            } else {
                new_promise.break_with(result, output, stderr);
            }
        } else {
            // The callback's own captured scalars are per-binding closure
            // state, exactly like a `start { }` block's: keep them off the
            // name-keyed shared lane, or a same-named lexical written on any
            // other thread (a caller's loop `my $job`) is pulled over the
            // callback's capture at its next sync point (`sleep`, `.result`).
            let mut thread_interp = self.clone_for_thread_for_block(&block);
            let orig_for_waiter = orig.clone();
            // Queue this callback on `orig` rather than spawning our own
            // thread that blocks on `orig.wait()`: whichever `keep`/
            // `break_with` call resolves `orig` runs every queued waiter in
            // registration order on one dedicated thread. That ordering
            // guarantee is what fixes S17-promise/then.t's intermittent CI
            // failure — a `.then` registered on `orig` alongside an
            // `.andthen`/`.orelse` chain (also rooted at `orig`) used to each
            // spawn an independent OS thread racing the same
            // `Condvar::notify_all` wake-up, so which one actually ran first
            // depended on OS scheduling and could reorder under load.
            orig.on_resolve(Box::new(move |status, result, output, stderr| {
                if should_run(&status) {
                    let promise_val = Value::promise(orig_for_waiter);
                    // Each queued waiter is a stack of its own (`$*STACK-ID`),
                    // though they share the resolving thread.
                    let cb_result = crate::runtime::stack_id::run_on_fresh_stack(|| {
                        thread_interp.vm_call_on_value(block, vec![promise_val], None)
                    });
                    let out = std::mem::take(&mut thread_interp.output_sink_mut().output);
                    let err = std::mem::take(&mut thread_interp.output_sink_mut().stderr_output);
                    thread_interp.resolve_promise_callback(
                        &new_promise,
                        cb_result,
                        format!("{}{}", output, out),
                        format!("{}{}", stderr, err),
                    );
                } else if propagate_kept {
                    new_promise.keep(result, output, stderr);
                } else {
                    new_promise.break_with(result, output, stderr);
                }
            }));
        }
        ret
    }

    /// `Promise.result`: wait for the promise to resolve (blocks while it is
    /// `Planned`); a broken promise re-throws its cause.
    // Cost: O(1) after the wait, plus the replay of settled `Proc::Async` taps.
    pub(crate) fn promise_result(&mut self, shared: &SharedPromise) -> Result<Value, RuntimeError> {
        let (result, _, _) = shared.wait();
        self.sync_shared_vars_to_env();
        // Blocking on a promise is a synchronization point: any output a
        // `.then`/`.andthen`/`.orelse` callback produced on its worker
        // thread must be flushed to real stdout before we return, so it
        // interleaves in the same order Rakudo produces (callback output
        // appears while the main thread is blocked in `.result`).
        self.drain_shared_thread_output();
        let status = shared.status();
        if status == "Broken" {
            // .result on a Broken promise re-throws the cause. A plain Str
            // reason (e.g. `Promise.break("oops")`) gets wrapped in X::AdHoc;
            // an object instance (a user exception the promise was broken
            // with, e.g. via a whenever LAST-phaser die propagated through
            // `SinkEvent::Quit`) passes through with its real type intact --
            // same fix as `.cause`, and the same shape-based check
            // `Supplier.quit()` already uses, since a name-based "is this an
            // exception" check cannot see a user class's `is Exception`
            // ancestry.
            let msg = result.to_string_value();
            let ex = if Self::is_exception_object(&result) {
                result
            } else {
                let mut attrs = HashMap::new();
                attrs.insert("payload".to_string(), Value::str(msg.clone()));
                attrs.insert("message".to_string(), Value::str(msg.clone()));
                Value::make_instance(Symbol::intern("X::AdHoc"), attrs)
            };
            // Rakudo composes `X::Promise::Broken` into the cause on the way
            // out of `.result`, so the thrown exception is the anonymous mixin
            // `X::AdHoc+{X::Promise::Broken}` (or `MyEx+{X::Promise::Broken}`
            // for a user exception) while `.cause` keeps handing back the
            // *un*mixed original. `.message`/`.Str` are unchanged by the
            // mixin; only `.gist` gains the "Tried to get the result of a
            // broken Promise" wrapper.
            let ex = self.compose_promise_broken_role(ex);
            let mut err = RuntimeError::new(msg);
            err.exception = Some(Box::new(ex));
            Err(err)
        } else {
            // Replay deferred taps for Proc::Async results -- the promise's
            // own, or those of a settled component of an allof/anyof
            // composite.
            self.replay_settled_proc_taps(shared, &result);
            Ok(result)
        }
    }

    /// `Promise.then` / `.andthen` / `.orelse`: a promise resolved by
    /// `block`'s outcome. `.then` always runs the block, `.andthen` only on a
    /// kept promise (it propagates a broken one), `.orelse` only on a broken
    /// one (it propagates a kept one).
    // Cost: O(1) excluding the callback, which runs user code.
    pub(crate) fn promise_chain(
        &mut self,
        shared: &SharedPromise,
        method: &str,
        block: Value,
    ) -> Result<Value, RuntimeError> {
        self.bind_callable_param("&code", &block)?;
        Ok(match method {
            "andthen" => self.promise_chain_method(shared, block, |s| s == "Kept", false),
            "orelse" => self.promise_chain_method(shared, block, |s| s == "Broken", true),
            _ => self.promise_chain_method(shared, block, |_| true, true),
        })
    }

    /// `Promise.keep` / `.break`: take the promise's single vow, then resolve
    /// it with `value` (`True` / `"Died"` when none is given). Rakudo's
    /// `keep`/`break` take the vow before resolving, so the *second* call
    /// through the Promise (and any call after an explicit `.vow`) reports
    /// X::Promise::Vowed rather than X::Promise::Resolved. Only a promise
    /// resolved behind mutsu's back (`Promise.kept`/`Promise.broken`, whose
    /// vow is never taken) can still reach the X::Promise::Resolved error.
    // Cost: O(s), s = the resolution's subscribers.
    pub(crate) fn promise_keep_or_break(
        &mut self,
        shared: &SharedPromise,
        kept: bool,
        value: Option<Value>,
    ) -> Result<Value, RuntimeError> {
        if !shared.take_vow() {
            return Err(Self::promise_vowed_error(shared));
        }
        self.promise_resolve_through_vow(shared, kept, value)
    }

    /// Resolve `shared` with `value` (`True` / `"Died"` when none is given),
    /// its vow already held: `Promise::Vow.keep`/`.break`, and the second half
    /// of `Promise.keep`/`.break`. Only an already-settled promise is an
    /// error.
    // Cost: O(s), s = the resolution's subscribers.
    pub(crate) fn promise_resolve_through_vow(
        &mut self,
        shared: &SharedPromise,
        kept: bool,
        value: Option<Value>,
    ) -> Result<Value, RuntimeError> {
        let value = value.unwrap_or_else(|| {
            if kept {
                Value::TRUE
            } else {
                Value::str_from("Died")
            }
        });
        match self.resolve_promise_dispatching(shared, kept, value, None)? {
            Ok(()) => Ok(Value::NIL),
            Err(status) => Err(Self::promise_resolved_error(shared, &status)),
        }
    }

    /// `Promise.cause`: the exception a broken promise holds;
    /// X::Promise::CauseOnlyValidOnBroken for any other status.
    // Cost: O(1).
    pub(crate) fn promise_cause(shared: &SharedPromise) -> Result<Value, RuntimeError> {
        let status = shared.status();
        if status != "Broken" {
            let mut attrs = HashMap::new();
            attrs.insert("status".to_string(), Value::str(status.clone()));
            attrs.insert("promise".to_string(), Value::promise(shared.clone()));
            let ex = Value::make_instance(
                Symbol::intern("X::Promise::CauseOnlyValidOnBroken"),
                attrs,
            );
            let mut err = RuntimeError::new(format!(
                "Can only call cause on a broken promise (status: {})",
                status
            ));
            err.exception = Some(Box::new(ex));
            return Err(err);
        }
        let (result, _, _) = shared.wait();
        Ok(Self::promise_cause_value(result))
    }

    /// `Promise.vow`: a promise has exactly one vow to give away. Taking it
    /// twice -- or taking it after `.keep`/`.break` already consumed it -- is
    /// X::Promise::Vowed.
    // Cost: O(1).
    pub(crate) fn promise_vow(shared: &SharedPromise, target: &Value) -> Result<Value, RuntimeError> {
        if !shared.take_vow() {
            return Err(Self::promise_vowed_error(shared));
        }
        let mut attrs = HashMap::new();
        attrs.insert("promise".to_string(), target.clone());
        Ok(Value::make_instance(Symbol::intern("Promise::Vow"), attrs))
    }

    /// `Channel.send`'s delivery, shared with a `Supply.Channel` forwarding
    /// tap (`SupplierEmitAction::ChannelSend`): the value is queued, never
    /// emitted behind the queue's back, and then the pump hands it to a tap of
    /// the channel's Supplies if one is ready -- those taps are consumers of the
    /// queue like `receive` (see `native_methods::channel_supply`). The
    /// forwarding tap used to queue it only, so
    /// `$supplier.Supply.Channel.Supply.tap(...)` (Cro's WebSocket handler
    /// feed) never saw a value emitted after the tap.
    // Cost: O(v * t), v = values the pump moves, t = the channel's attached taps.
    pub(crate) fn channel_send_value(&mut self, ch: &SharedChannel, value: Value) {
        ch.send(value);
        // A tap callback that dies does not fail the sender (see
        // `pump_channel_taps`); only completing a tap can error, and that
        // happens on close, not here.
        let _ = self.pump_channel_taps(ch);
    }

    /// `Channel.send`: queue `value`; X::Channel::SendOnClosed on a closed
    /// channel.
    // Cost: O(v * t), as `channel_send_value`.
    pub(crate) fn channel_send(&mut self, ch: &SharedChannel, value: Value) -> Result<Value, RuntimeError> {
        if !ch.can_send() {
            return Err(Self::channel_send_closed_error());
        }
        self.channel_send_value(ch, value);
        Ok(Value::NIL)
    }

    /// `Channel.receive`: the next value, blocking until one arrives;
    /// X::Channel::ReceiveOnClosed once the channel is closed and drained, the
    /// failure once it failed.
    // Cost: O(1) after the wait.
    pub(crate) fn channel_receive(ch: &SharedChannel) -> Result<Value, RuntimeError> {
        match ch.receive_result() {
            Ok(v) if v.is_nil() => Err(Self::channel_receive_closed_error()),
            Ok(value) => Ok(value),
            Err(cause) => {
                let ex = Self::as_exception_value(cause);
                let mut err = RuntimeError::new(ex.to_string_value());
                err.exception = Some(Box::new(ex));
                Err(err)
            }
        }
    }

    /// `Channel.close` / `.fail($reason)`: closing (or failing) the channel
    /// completes each tap of its Supplies once the values still queued have
    /// been delivered.
    // Cost: O(v * t), v = values still queued, t = the channel's taps.
    pub(crate) fn channel_close(
        &mut self,
        ch: &SharedChannel,
        failure: Option<Value>,
    ) -> Result<Value, RuntimeError> {
        match failure {
            Some(reason) => ch.fail(Self::as_exception_value(reason)),
            None => ch.close(),
        }
        self.pump_channel_taps(ch)?;
        Ok(Value::NIL)
    }

    /// `Channel.list`: a lazy list that pulls one `receive` per element, on
    /// demand, so a consumer sees each item as it arrives (#12727).
    // Cost: O(1).
    pub(crate) fn channel_list(ch: &SharedChannel) -> Value {
        let mut ll = crate::value::LazyList::new_cat_pull(
            Value::channel(ch.clone()),
            crate::value::CatPullMode::ChannelReceive,
        );
        ll.list_context = true;
        Value::lazy_list(crate::gc::Gc::new(ll))
    }

    /// `Any`'s `Seq`, `List` and `Array` on a channel (`self.list.METHOD`):
    /// `.Seq` pulls one `receive` per element, on demand, like `.list`;
    /// `.List` and `.Array` are the eager forms and drain until the close.
    // Cost: O(1) for `Seq`; O(v) for the eager forms, v = values received.
    pub(crate) fn channel_list_coercion(
        ch: &SharedChannel,
        method: &str,
    ) -> Result<Value, RuntimeError> {
        if method == "Seq" {
            let ll = crate::value::LazyList::new_cat_pull(
                Value::channel(ch.clone()),
                crate::value::CatPullMode::ChannelReceive,
            );
            return Ok(Value::lazy_list(crate::gc::Gc::new(ll)));
        }
        let mut items = Vec::new();
        loop {
            match ch.receive_result() {
                Ok(v) if v.is_nil() => break,
                Ok(value) => items.push(value),
                Err(cause) => {
                    let ex = Self::as_exception_value(cause);
                    let mut err = RuntimeError::new(ex.to_string_value());
                    err.exception = Some(Box::new(ex));
                    return Err(err);
                }
            }
        }
        Ok(if method == "Array" {
            Value::real_array(items)
        } else {
            Value::array(items)
        })
    }

    /// Seed the native promise or channel an instance of a user subclass of
    /// `Promise` or `Channel` carries (#13634): Rakudo's subclass *is* a
    /// promise (`await`, `.then`, `.vow` all work on it), so the rows answer
    /// its built-in methods on this payload. A promise is bound to the
    /// scheduler a `:scheduler` argument names, as `Promise.new`'s is.
    // Cost: O(m), m = classes in the MRO.
    pub(crate) fn seed_concurrency_payload(
        &mut self,
        attrs: &mut AttrMap,
        class_mro: &[Symbol],
        args: &[Value],
    ) {
        use crate::builtins::method_table::promise_channel::{CHANNEL_PAYLOAD, PROMISE_PAYLOAD};
        let Some(class) = class_mro.first().copied() else {
            return;
        };
        if class_mro.iter().any(|name| name == "Promise") && !attrs.contains_key(PROMISE_PAYLOAD) {
            let explicit = Self::named_value(args, "scheduler");
            let promise = self.new_bound_promise(class, explicit);
            attrs.insert(PROMISE_PAYLOAD, Value::promise(promise));
        } else if class_mro.iter().any(|name| name == "Channel")
            && !attrs.contains_key(CHANNEL_PAYLOAD)
        {
            attrs.insert(CHANNEL_PAYLOAD, Value::channel(SharedChannel::new()));
        }
    }

    /// `Any`'s `Seq`/`List`/`Array` on an instance of a user subclass of
    /// `Channel`: read on the channel it carries (#13634), before `Any`'s
    /// rows read the instance as one item. `None` for any other call.
    // Cost: O(1) unless it answers; then as `channel_list_coercion`.
    pub(crate) fn channel_subclass_coercion(
        target: &Value,
        method: &str,
    ) -> Option<Result<Value, RuntimeError>> {
        if !crate::builtins::method_table::CHANNEL_LIST_COERCIONS.contains(&method)
            || !matches!(target.view(), ValueView::Instance { .. })
        {
            return None;
        }
        let receiver = crate::builtins::method_table::promise_channel_receiver_of(target)?;
        let ValueView::Channel(ch) = receiver.view() else {
            return None;
        };
        Some(Self::channel_list_coercion(&ch, method))
    }
}

