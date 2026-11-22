//! `Supply.throttle` over a live source (ADR-18443, #18579).
//!
//! rakudo's `throttle($limit, $seconds)` passes at most `$limit` values per
//! `$seconds` window and queues the rest for the windows after it; `done`
//! waits until the queue is drained. `throttle($limit, &block)` runs the block
//! on each value with at most `$limit` jobs at once and emits one Promise per
//! job, kept with the block's result. The finite path runs over materialized
//! values; this is the live stage, built per tap by `supply_derive_eager`.

use super::native_methods::*;
use super::*;
use crate::symbol::Symbol;
use crate::value::SharedPromise;
use crate::value::waker::SinkEvent;
use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, Ordering};

/// Whether `args` are a throttle this live stage implements: a limit and a
/// window or a block, without `:control` / `:status` / `:vent-at`.
// Cost: O(a), a = the arguments.
pub(in crate::runtime) fn throttle_is_live_stage(args: &[Value]) -> bool {
    !args.iter().any(|a| {
        matches!(a.view(), ValueView::Pair(key, _)
            if matches!(key.as_str(), "control" | "status" | "vent-at" | "scheduler"))
    })
}

/// Emit `value` on the supplier `sid` from a stage worker.
// Cost: O(t), t = taps of the supplier.
fn emit_on(interp: &mut Interpreter, sid: u64, value: Value) {
    supplier_emit(sid, value.clone());
    let actions = supplier_emit_callbacks(sid, &value);
    let _ = interp.drive_supplier_emit_actions(sid, actions);
}

impl Interpreter {
    /// The live throttle stage over the supplier `source_id` with the
    /// throttle's `args`: a Supply over a fresh supplier that a worker feeds.
    // Cost: O(1) here; O(1) amortized per value on the worker, plus a block
    // call per value for the block form.
    pub(in crate::runtime) fn throttle_live_supply(&mut self, source_id: u64, args: &[Value]) -> Value {
        let positionals: Vec<Value> = args
            .iter()
            .filter(|a| !matches!(a.view(), ValueView::Pair(..)))
            .cloned()
            .collect();
        let limit = positionals.first().map(|v| v.to_f64() as usize).unwrap_or(1).max(1);
        let block = positionals
            .get(1)
            .filter(|v| matches!(v.view(), ValueView::Sub(_) | ValueView::WeakSub(_)))
            .cloned();
        let window = std::time::Duration::from_secs_f64(
            positionals.get(1).filter(|_| block.is_none()).map(Value::to_f64).unwrap_or(0.0).max(0.0),
        );
        let out_sid = next_supplier_id();
        let mut thread_interp = self.clone_for_thread();
        let waker = crate::value::waker::ReactWaker::new();
        let sink_id = supplier_sink_register(source_id, 0, &waker, true);
        crate::runtime::worker_pool::submit(move || {
            let mut queue: std::collections::VecDeque<Value> = std::collections::VecDeque::new();
            let running = Arc::new(AtomicUsize::new(0));
            let mut window_start = crate::thread_compat::Instant::now();
            let mut sent_in_window = 0usize;
            let mut quit: Option<Value> = None;
            let mut finished = false;
            loop {
                for (_, event, _) in waker.drain() {
                    match event {
                        SinkEvent::Emit(v) => queue.push_back(v),
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
                match &block {
                    // A window: up to `limit` values per window.
                    None => {
                        let now = crate::thread_compat::Instant::now();
                        if now.duration_since(window_start) >= window {
                            window_start = now;
                            sent_in_window = 0;
                        }
                        while sent_in_window < limit
                            && let Some(v) = queue.pop_front()
                        {
                            sent_in_window += 1;
                            emit_on(&mut thread_interp, out_sid, v);
                        }
                    }
                    // A block: up to `limit` jobs at once, a Promise each.
                    Some(block) => {
                        while running.load(Ordering::Acquire) < limit
                            && let Some(v) = queue.pop_front()
                        {
                            running.fetch_add(1, Ordering::AcqRel);
                            let promise = SharedPromise::new();
                            emit_on(&mut thread_interp, out_sid, Value::promise(promise.clone()));
                            let mut job_interp = thread_interp.clone_for_thread_for_block(block);
                            let block = block.clone();
                            let running = Arc::clone(&running);
                            let job_waker = waker.clone();
                            crate::runtime::worker_pool::submit(move || {
                                match job_interp.call_supply_callback(block, vec![v]) {
                                    Ok(result) => promise.keep(result, String::new(), String::new()),
                                    Err(err) => {
                                        let reason = err.exception.map(|e| *e).unwrap_or_else(|| Value::str(err.message.to_string()));
                                        promise.break_with(reason, String::new(), String::new())
                                    }
                                }
                                running.fetch_sub(1, Ordering::AcqRel);
                                job_waker.notify();
                            });
                        }
                    }
                }
                if finished && queue.is_empty() && running.load(Ordering::Acquire) == 0 {
                    break;
                }
                // Sleep until the next window opens, a job ends, or the
                // source moves.
                let wait = if block.is_none() && !queue.is_empty() {
                    window.saturating_sub(crate::thread_compat::Instant::now().duration_since(window_start))
                } else {
                    std::time::Duration::from_millis(100)
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
