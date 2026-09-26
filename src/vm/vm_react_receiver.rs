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
                        self.supply_emit_buffer.push(Vec::new());
                        let cb_result =
                            self.call_react_callback(&sub.callback.clone(), vec![value]);
                        let emitted = self.supply_emit_buffer.pop().unwrap_or_default();
                        for item in emitted {
                            // A `whenever` nested in this body registered
                            // its subscription marker into the same
                            // frame. It is not a value the supply
                            // emitted: hand it to the adoption queue
                            // instead of letting it become the promise's
                            // result.
                            if Self::is_whenever_subscription_marker(&item) {
                                self.pending_react_subscriptions.push(item);
                            } else {
                                *last_value = item;
                            }
                        }
                        if promise.is_resolved() {
                            return Ok(ReceiverFlow::Return);
                        }
                        if let Err(err) = cb_result {
                            // `done`/`last` inside the whenever complete the
                            // supply: keep the promise with the last emitted
                            // value immediately rather than spinning to the
                            // deadline.
                            if err.is_react_done() || err.is_last() || err.is_supply_body_done() {
                                promise.keep(last_value.clone(), String::new(), String::new());
                                return Ok(ReceiverFlow::Return);
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
                                return Ok(ReceiverFlow::Return);
                            }
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
                if matches!(policy, SupplyDrivePolicy::Promise { .. }) {
                    // On the await path an inner quit just retires the
                    // receiver; the promise is resolved/broken elsewhere.
                    sub.done = true;
                } else {
                    let mut handled = false;
                    for quit_cb in &sub.quit_callbacks {
                        self.call_supply_quit_handler(quit_cb.clone(), error.clone())?;
                        handled = true;
                    }
                    sub.done = true;
                    if !handled {
                        let ch_quit_err =
                            crate::runtime::Interpreter::runtime_error_from_supply_reason(error);
                        return Err(crate::runtime::Interpreter::wrap_react_died(ch_quit_err));
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
}
