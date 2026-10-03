//! Streaming a tapped `supply { }` block's plain `emit`s to the tap (#11434).
//!
//! `run_on_demand_body` runs the block inside an emit-buffer frame. Before
//! this, a plain `.tap` saw every value only once the body had returned, when
//! the frame was replayed — so `supply { emit 'a'; sleep 0.3; emit 'b' }`
//! delivered `a` 0.3 s late. Rakudo runs the tap callback inside each `emit`.
//!
//! A tap now installs a [`TapStream`] on the body's frame. An `emit` on the
//! frame's own emitter hands the value straight to the tap (its `do`
//! callbacks, delay and throttle included) instead of buffering it. The
//! `whenever` subscription markers the body registers still go to the buffer
//! and are wired up after the body returns, which is also Rakudo's order: a
//! `whenever` in a supply block starts only once the block's setup finished,
//! so `supply { whenever $cold { emit $_ }; emit 3 }` delivers `3` first.
use super::*;

/// What a tap does with the values its on-demand body emits while it runs.
#[derive(Debug)]
pub(crate) struct TapStream {
    pub(crate) tap_cb: Value,
    pub(crate) do_cbs: Vec<Value>,
    pub(crate) delay_seconds: f64,
    pub(crate) throttle_limit: Option<usize>,
    /// Values delivered so far (paces `throttle_limit`).
    pub(crate) delivered: usize,
    /// The tap callback ran `done`/`last`: later values are dropped.
    pub(crate) stopped: bool,
    /// The tap callback died; the error unwinds the body and leaves `.tap`.
    pub(crate) failed: bool,
    /// `active_supply_emitters.len()` before the body's emitter was pushed.
    pub(crate) emitters_base: usize,
}

/// How delivering one value to a tap ended.
pub(crate) enum TapStep {
    Continue,
    /// The tap callback ran `done`/`last`: stop delivering.
    Stop,
}

impl TapStream {
    // Cost: O(d), d = number of `do` callbacks.
    pub(crate) fn new(
        tap_cb: Value,
        do_cbs: Vec<Value>,
        delay_seconds: f64,
        throttle_limit: Option<usize>,
    ) -> Self {
        Self {
            tap_cb,
            do_cbs,
            delay_seconds,
            throttle_limit,
            delivered: 0,
            stopped: false,
            failed: false,
            emitters_base: 0,
        }
    }
}

impl Interpreter {
    /// Deliver the `idx`-th value of a tap: pace it (`delay_seconds`, or one
    /// pause per `throttle_limit` values), run the `do` callbacks, then the tap
    /// callback. Shared by the post-body replay and the streaming `emit`.
    // Cost: O(d + t), d = `do` callbacks, t = cost of the tap callback.
    pub(crate) fn deliver_tap_value(
        &mut self,
        tap_cb: &Value,
        do_cbs: &[Value],
        delay_seconds: f64,
        throttle_limit: Option<usize>,
        idx: usize,
        v: &Value,
    ) -> Result<TapStep, RuntimeError> {
        if let Some(limit) = throttle_limit {
            if limit > 0 && idx.is_multiple_of(limit) {
                Self::sleep_for_supply_delay(delay_seconds);
            }
        } else {
            Self::sleep_for_supply_delay(delay_seconds);
        }
        for cb in do_cbs {
            self.call_sub_value(cb.clone(), vec![v.clone()], true)?;
        }
        if !Self::supply_has_active_callback(tap_cb) {
            return Ok(TapStep::Continue);
        }
        // A `done`/`last` inside the tap callback completes the tap cleanly:
        // stop emitting and fall through to the done callback. It must not
        // surface as a runtime error (this is how
        // `(1..Inf).Supply.tap({ ...; done if ... })` terminates). The `match`
        // below handles `is_react_done()`/`is_last()` raised anywhere in this
        // call's dynamic extent — see `runtime::react_done_handler_depth`.
        let react_done_handler =
            crate::runtime::react_done_handler_depth::ReactDoneHandlerGuard::new();
        // A `whenever` body driven by a chained on-demand source is a stamped
        // callback: make its own supply block's emitter the innermost active
        // one while it runs, as `call_supply_tap` does, so a bare `emit` in a
        // sub the body calls reaches that block (TAP's `parse-stream` emits
        // from a nested `sub emit-reset`).
        let (own_emitter, stamped) = Self::whenever_tap_emitter(tap_cb);
        let own_emitter = own_emitter.filter(|_| stamped);
        if let Some(ref e) = own_emitter {
            self.async_state.active_supply_emitters.push(e.clone());
        }
        let tap_result = self.call_sub_value(tap_cb.clone(), vec![v.clone()], true);
        if own_emitter.is_some() {
            self.async_state.active_supply_emitters.pop();
        }
        drop(react_done_handler);
        match tap_result {
            Ok(_) => Ok(TapStep::Continue),
            Err(err) if err.is_react_done() || err.is_last() || err.is_supply_body_done() => {
                Ok(TapStep::Stop)
            }
            // `next` inside a whenever body (this tap callback is the body when
            // a chained on-demand supply drives it) skips the rest of the body
            // for THIS value only.
            Err(err) if err.is_next() => Ok(TapStep::Continue),
            Err(err) => Err(err),
        }
    }

    /// Collect an emission on the supplier `sid` into the innermost emit
    /// frame it belongs to — or, when that frame streams to a tap, deliver it
    /// to the tap right away. A no-op when no frame takes the emission.
    // Cost: O(1) when buffering; the tap delivery when streaming.
    pub(super) fn supply_emit_collect(
        &mut self,
        sid: Option<u64>,
        value: &Value,
    ) -> Result<(), RuntimeError> {
        let streams = match self.async_state.supply_emit_buffer.last() {
            Some(frame) => {
                frame.tap_stream.is_some() && frame.owner.is_some() && frame.owner == sid
            }
            None => false,
        };
        if !streams {
            if let Some(buf) = self.supply_emit_frame_for(sid) {
                buf.push(value.clone());
            }
            return Ok(());
        }
        self.stream_to_tap(value)
    }

    /// Hand `value` to the tap streaming from the innermost emit frame.
    ///
    /// The tap callback runs exactly as it did when the frame was replayed
    /// after the body: with the body's frame and its emitters set aside, so
    /// an `emit` it makes (a stamped `whenever` body re-emitting into its own
    /// supply block) reaches that block rather than this body's buffer.
    // Cost: O(d + t), see `deliver_tap_value`.
    fn stream_to_tap(&mut self, value: &Value) -> Result<(), RuntimeError> {
        let Some(mut frame) = self.async_state.supply_emit_buffer.pop() else {
            return Ok(());
        };
        let Some(mut stream) = frame.tap_stream.take() else {
            self.async_state.supply_emit_buffer.push(frame);
            return Ok(());
        };
        if stream.stopped {
            frame.tap_stream = Some(stream);
            self.async_state.supply_emit_buffer.push(frame);
            return Ok(());
        }
        let base = stream
            .emitters_base
            .min(self.async_state.active_supply_emitters.len());
        let body_emitters = self.async_state.active_supply_emitters.split_off(base);
        let idx = stream.delivered;
        stream.delivered += 1;
        let step = self.deliver_tap_value(
            &stream.tap_cb,
            &stream.do_cbs,
            stream.delay_seconds,
            stream.throttle_limit,
            idx,
            value,
        );
        self.async_state
            .active_supply_emitters
            .extend(body_emitters);
        let result = match step {
            Ok(TapStep::Continue) => Ok(()),
            Ok(TapStep::Stop) => {
                stream.stopped = true;
                Ok(())
            }
            Err(err) => {
                stream.stopped = true;
                stream.failed = true;
                Err(err)
            }
        };
        frame.tap_stream = Some(stream);
        self.async_state.supply_emit_buffer.push(frame);
        result
    }
}
