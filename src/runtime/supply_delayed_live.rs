//! `Supply.delayed` over a live source (#18608).
//!
//! rakudo's `delayed($time)` is
//! `supply { whenever self -> \val { $scheduler.cue: { emit val }, :in($time) } }`:
//! the emitter returns at once, each value reaches the taps `$time` seconds
//! after it arrived, in arrival order, and `done` follows the last pending
//! value. A worker owns the queue of pending values; the emitter only hands a
//! value to the worker's sink. The stage is built per tap by
//! `supply_derive_eager`, as `stable` and `throttle` are.

use super::native_methods::*;
use super::*;
use crate::symbol::Symbol;
use crate::value::waker::SinkEvent;
use std::collections::VecDeque;

impl Interpreter {
    /// The live delay stage over the supplier `source_id`: a Supply over a
    /// fresh supplier a worker feeds each source value `seconds` after it
    /// arrived. `done` waits for the pending values; `quit` drops them.
    // Cost: O(1) here; O(1) per value on the worker, plus the taps' calls.
    pub(in crate::runtime) fn delayed_live_supply(&mut self, source_id: u64, seconds: f64) -> Value {
        let delay = std::time::Duration::from_secs_f64(seconds.max(0.0));
        let out_sid = next_supplier_id();
        let mut thread_interp = self.clone_for_thread();
        // Registers with the source before returning, so nothing the caller
        // emits right after `.delayed` is missed.
        let waker = crate::value::waker::ReactWaker::new();
        let sink_id = supplier_sink_register(source_id, 0, &waker, true);
        crate::runtime::worker_pool::submit(move || {
            let mut pending: VecDeque<(Value, crate::thread_compat::Instant)> = VecDeque::new();
            let mut quit: Option<Value> = None;
            let mut finished = false;
            loop {
                for (_, event, _) in waker.drain() {
                    match event {
                        SinkEvent::Emit(v) => {
                            pending.push_back((v, crate::thread_compat::Instant::now() + delay));
                        }
                        SinkEvent::Done => finished = true,
                        SinkEvent::Quit(reason) => {
                            quit = Some(reason);
                            finished = true;
                        }
                    }
                }
                if quit.is_some() {
                    break;
                }
                while pending
                    .front()
                    .is_some_and(|(_, due)| *due <= crate::thread_compat::Instant::now())
                {
                    let Some((v, _)) = pending.pop_front() else { break };
                    supplier_emit(out_sid, v.clone());
                    let actions = supplier_emit_callbacks(out_sid, &v);
                    let _ = thread_interp.drive_supplier_emit_actions(out_sid, actions);
                }
                if finished && pending.is_empty() {
                    break;
                }
                // Sleep until the next value is due or the source moves.
                let wait = match pending.front() {
                    Some((_, due)) => due.duration_since(crate::thread_compat::Instant::now()),
                    None => std::time::Duration::from_millis(100),
                };
                if !wait.is_zero() {
                    waker.wait_activity(wait);
                }
            }
            supplier_sink_unregister(source_id, sink_id);
            if let Some(reason) = quit {
                supplier_quit(out_sid, reason.clone());
                let _ = thread_interp.propagate_supplier_quit(out_sid, reason);
            } else {
                supplier_done(out_sid);
                for done_cb in take_supplier_done_callbacks(out_sid) {
                    let _ = thread_interp.call_sub_value(done_cb, Vec::new());
                }
            }
        });
        let mut attrs = HashMap::new();
        attrs.insert("values".to_string(), Value::array(Vec::new()));
        attrs.insert("taps".to_string(), Value::array(Vec::new()));
        attrs.insert("live".to_string(), Value::TRUE);
        attrs.insert("supplier_id".to_string(), Value::int(out_sid as i64));
        Value::make_instance(Symbol::intern("Supply"), attrs)
    }
}
