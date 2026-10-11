//! Delivery of one channel-receiver event to its `whenever` inside the react /
//! supply drive loop (`drive_react_subscriptions_loop`), split from
//! `vm_react_subscriptions`.
//!
//! Receiver-backed subscriptions (`whenever <Promise>`, `Proc::Async` output,
//! signal/socket supplies, ...) are polled once per round in index order. That
//! order is not the order the events were sent in, so delivering them as polled
//! could let a later event end the react before an earlier one on another
//! receiver was seen (issue #9611: `whenever $p.start { done }` beating the
//! final `$p.stdout` chunk the reader had queued *before* `.start` was kept).
//! Every channel event carries a global send sequence, and
//! [`Interpreter::deliver_receiver_poll_ordered`] delivers the events older
//! than the one in hand first -- a merge in true send order that cannot starve
//! anything, since a busy producer only ever adds younger events.
use super::*;
use crate::runtime::native_methods::SupplyEvent;
use crate::runtime::react_whenever::{ReactSubscription, SupplyDrivePolicy};
use crate::value::waker::ReactWaker;
use std::sync::mpsc;

/// What the drive loop does after a receiver event was delivered.
pub(super) enum ReceiverFlow {
    /// Keep driving.
    Continue,
    /// A consumer ended the react (`done`): leave the loop normally.
    EndReact,
    /// The awaited promise (`SupplyDrivePolicy::Promise`) resolved: return.
    Return,
}

impl Interpreter {
    /// Deliver `poll` -- the event just taken from `react_subs[si]`'s receiver --
    /// after first delivering every event on the *other* receivers that was
    /// sent before it, oldest first.
    // Cost: O(e * r), e = older events flushed, r = receiver subscriptions.
    pub(super) fn deliver_receiver_poll_ordered(
        &mut self,
        react_subs: &mut [ReactSubscription],
        si: usize,
        poll: Result<(u64, SupplyEvent), mpsc::TryRecvError>,
        policy: &mut SupplyDrivePolicy,
        waker: &ReactWaker,
        progressed: &mut bool,
    ) -> Result<ReceiverFlow, RuntimeError> {
        if let Ok((seq, _)) = &poll {
            while let Some(oldest) = Self::oldest_receiver_event_before(react_subs, si, *seq) {
                let Some(earlier) = react_subs[oldest].receiver.as_ref().map(|r| r.try_recv())
                else {
                    break;
                };
                match self
                    .deliver_receiver_poll(react_subs, oldest, earlier, policy, waker, progressed)?
                {
                    ReceiverFlow::Continue => {}
                    flow => return Ok(flow),
                }
            }
            if react_subs[si].done {
                return Ok(ReceiverFlow::Continue);
            }
        }
        self.deliver_receiver_poll(
            react_subs,
            si,
            poll.map(|(_, event)| event),
            policy,
            waker,
            progressed,
        )
    }

    /// The live receiver subscription other than `si` whose next event was sent
    /// earliest, if that is before `seq`.
    // Cost: O(r), r = receiver subscriptions.
    fn oldest_receiver_event_before(
        react_subs: &[ReactSubscription],
        si: usize,
        seq: u64,
    ) -> Option<usize> {
        let mut oldest: Option<(u64, usize)> = None;
        for (j, sub) in react_subs.iter().enumerate() {
            if j == si || sub.done || sub.supplier_id.is_some() || sub.channel.is_some() {
                continue;
            }
            if let Some(next) = sub.receiver.as_ref().and_then(|r| r.peek_seq())
                && next < seq
                && oldest.is_none_or(|(o, _)| next < o)
            {
                oldest = Some((next, j));
            }
        }
        oldest.map(|(_, j)| j)
    }

    /// Deliver one receiver poll result to `react_subs[si]`'s consumer.
    // Cost: O(1) plus the consumer callback.
    fn deliver_receiver_poll(
        &mut self,
        react_subs: &mut [ReactSubscription],
        si: usize,
        poll: Result<SupplyEvent, mpsc::TryRecvError>,
        policy: &mut SupplyDrivePolicy,
        waker: &ReactWaker,
        progressed: &mut bool,
    ) -> Result<ReceiverFlow, RuntimeError> {
        // Raku ordering guarantee: values `emit`ted into a supplier
        // *before* the event this receiver just delivered are causally
        // earlier and must reach their `whenever`s first — even when that
        // event's callback ends the react (e.g. `whenever start { emit … }`
        // finishing while a sibling `whenever` calls `done`, so the sibling
        // supplier's already-emitted values would otherwise be lost). Drain
        // the waker queue before running this receiver's consumer, so
        // their pending values are delivered in source order.
        if matches!(poll, Ok(SupplyEvent::Emit(_)))
            && matches!(policy, SupplyDrivePolicy::React)
            && self.dispatch_waker_events(waker, react_subs, progressed, policy)?
        {
            return Ok(ReceiverFlow::EndReact);
        }
        let sub = &mut react_subs[si];
        match poll {
            Ok(SupplyEvent::Emit(value)) => {
                *progressed = true;
                match policy {
                    SupplyDrivePolicy::Promise {
                        promise,
                        last_value,
                        ..
                    } => {
                        // Capture values the whenever block `emit`s so a
                        // later `done` resolves the promise with the last one.
                        let callback = sub.callback.clone();
                        if self.run_promise_policy_callback(promise, last_value, |vm| {
                            vm.call_react_callback(&callback, vec![value]).map(|_| ())
                        }) {
                            return Ok(ReceiverFlow::Return);
                        }
                    }
                    SupplyDrivePolicy::React => {
                        if sub.is_lines {
                            let chunk = value.to_string_value();
                            sub.line_buffer.push_str(&chunk);
                            while let Some(pos) = sub.line_buffer.find('\n') {
                                let line = sub.line_buffer[..pos].to_string();
                                sub.line_buffer = sub.line_buffer[pos + 1..].to_string();
                                if self.run_react_consumer(sub, Value::str(line))? {
                                    return Ok(ReceiverFlow::EndReact);
                                }
                                if sub.done {
                                    break;
                                }
                            }
                        } else if self.run_react_consumer(sub, value)? {
                            return Ok(ReceiverFlow::EndReact);
                        }
                    }
                }
            }
            Ok(SupplyEvent::Done) => {
                *progressed = true;
                if matches!(policy, SupplyDrivePolicy::Promise { .. }) {
                    // Inner supply done: the promise resolves through the
                    // supplier registry, not the channel close — just
                    // retire this receiver.
                    sub.done = true;
                } else {
                    if sub.is_lines && !sub.line_buffer.is_empty() {
                        let remaining = std::mem::take(&mut sub.line_buffer);
                        match self
                            .call_react_callback(&sub.callback.clone(), vec![Value::str(remaining)])
                        {
                            Err(e) if e.is_react_done() => return Ok(ReceiverFlow::EndReact),
                            other => {
                                other?;
                            }
                        }
                    }
                    for callback in &sub.last_callbacks {
                        self.call_react_callback(&callback.clone(), Vec::new())?;
                    }
                    sub.done = true;
                }
            }
            Ok(SupplyEvent::Quit(error)) => {
                *progressed = true;
                if let SupplyDrivePolicy::Promise {
                    promise,
                    last_value,
                    ..
                } = policy
                {
                    sub.done = true;
                    // A source that quits quits the supply block, unless the
                    // whenever's QUIT phaser handles it — the same rule the
                    // react path below applies (#12789). Leaving the promise
                    // alone parked `Promise(supply { whenever $broken {...} })`
                    // until the drive's deadline and then kept it.
                    if sub.quit_callbacks.is_empty() {
                        promise.break_with(error, String::new(), String::new());
                        return Ok(ReceiverFlow::Return);
                    }
                    let quit_cbs = sub.quit_callbacks.clone();
                    if self.run_promise_policy_callback(promise, last_value, |vm| {
                        for quit_cb in quit_cbs {
                            vm.call_whenever_quit_phaser(quit_cb, error.clone())?;
                        }
                        Ok(())
                    }) {
                        return Ok(ReceiverFlow::Return);
                    }
                } else {
                    let mut handled = false;
                    for quit_cb in sub.quit_callbacks.clone() {
                        if let Err(err) = self.call_whenever_quit_phaser(quit_cb, error.clone()) {
                            sub.done = true;
                            self.whenever_quit_failed(err, policy)?;
                            return Ok(ReceiverFlow::Return);
                        }
                        handled = true;
                    }
                    sub.done = true;
                    if !handled {
                        let ch_quit_err =
                            crate::runtime::Interpreter::runtime_error_from_supply_reason(error);
                        return Err(self.wrap_react_died(ch_quit_err));
                    }
                }
            }
            Err(mpsc::TryRecvError::Empty) => {}
            Err(mpsc::TryRecvError::Disconnected) => {
                sub.done = true;
                *progressed = true;
            }
        }
        Ok(ReceiverFlow::Continue)
    }

    /// Run a `whenever` body or QUIT phaser of a supply block driven for its
    /// Promise (`SupplyDrivePolicy::Promise`), capturing what it `emit`s:
    /// the last emitted value becomes `last_value`, a nested `whenever`'s
    /// subscription marker goes to the adoption queue, `done`/`last` keeps
    /// the promise and a `die` breaks it. Returns whether the promise is
    /// resolved, so the drive should stop.
    // Cost: O(e) beyond the callback itself, e = values it emits.
    fn run_promise_policy_callback(
        &mut self,
        promise: &crate::value::SharedPromise,
        last_value: &mut Value,
        run: impl FnOnce(&mut Self) -> Result<(), RuntimeError>,
    ) -> bool {
        self.async_state
            .supply_emit_buffer
            .push(crate::runtime::EmitFrame::default());
        let cb_result = run(self);
        let emitted = self
            .async_state
            .supply_emit_buffer
            .pop()
            .unwrap_or_default()
            .values;
        for item in emitted {
            // A `whenever` nested in this body registered its subscription
            // marker into the same frame. It is not a value the supply
            // emitted: hand it to the adoption queue instead of letting it
            // become the promise's result.
            if Self::is_whenever_subscription_marker(&item) {
                self.async_state.pending_react_subscriptions.push(item);
            } else {
                *last_value = item;
            }
        }
        if promise.is_resolved() {
            return true;
        }
        if let Err(err) = cb_result {
            // `done`/`last` complete the supply: keep the promise with the
            // last emitted value immediately rather than spinning to the
            // deadline.
            if err.is_react_done() || err.is_last() || err.is_supply_body_done() {
                promise.keep(last_value.clone(), String::new(), String::new());
                return true;
            }
            // `next`/`redo` are loop control, not completion.
            if !err.is_next() && !err.is_redo() {
                // A `die` quits the supply: break with the cause.
                let cause = err
                    .exception
                    .as_deref()
                    .cloned()
                    .unwrap_or_else(|| Value::str(err.message.to_string()));
                promise.break_with(cause, String::new(), String::new());
                return true;
            }
        }
        false
    }

    /// Run a `whenever`'s `QUIT` phaser with the quit `reason` as `$_`. Like
    /// `CATCH`, the phaser handles the exception only when a `when`/`default`
    /// matches (or it `succeed`s); otherwise the exception is rethrown, so it
    /// quits the enclosing `react`/`supply` block — rakudo's behaviour, and
    /// what Cro's client relies on when its request `QUIT` matches neither
    /// of its two `when`s (#12789). Tap `quit => { ... }` callbacks are plain
    /// blocks and go through `call_supply_quit_handler` instead.
    // Cost: O(1) beyond running the phaser body.
    pub(crate) fn call_whenever_quit_phaser(
        &mut self,
        quit_cb: Value,
        reason: Value,
    ) -> Result<(), RuntimeError> {
        let saved_when = self.when_matched();
        loan_env!(self, set_when_matched(false));
        let result = self.call_react_callback(&quit_cb, vec![reason.clone()]);
        let matched = self.when_matched();
        loan_env!(self, set_when_matched(saved_when));
        match result {
            Ok(_) if matched => Ok(()),
            Ok(_) => Err(crate::runtime::Interpreter::runtime_error_from_supply_reason(reason)),
            Err(err) if err.is_succeed() => Ok(()),
            Err(err) => Err(err),
        }
    }

    /// A `whenever`'s `QUIT` phaser ended with `err` — a rethrown or new
    /// exception, or `done`. Under the Promise policy the supply block
    /// completes (`done` keeps the promise with the last emitted value, an
    /// exception breaks it) and `Ok(())` tells the caller to stop driving; under
    /// the react policy `done` propagates as is and an exception dies the react.
    // Cost: O(1), plus the `X::React::Died` wrap on the react path.
    pub(super) fn whenever_quit_failed(
        &mut self,
        err: RuntimeError,
        policy: &SupplyDrivePolicy,
    ) -> Result<(), RuntimeError> {
        match policy {
            SupplyDrivePolicy::Promise {
                promise,
                last_value,
                ..
            } => {
                if err.is_react_done() || err.is_supply_body_done() {
                    promise.keep(last_value.clone(), String::new(), String::new());
                } else {
                    let cause = err
                        .exception
                        .as_deref()
                        .cloned()
                        .unwrap_or_else(|| Value::str(err.message.to_string()));
                    promise.break_with(cause, String::new(), String::new());
                }
                Ok(())
            }
            SupplyDrivePolicy::React => {
                if err.is_react_done() || err.is_return() {
                    return Err(err);
                }
                Err(self.wrap_react_died_if_needed(err))
            }
        }
    }
}
