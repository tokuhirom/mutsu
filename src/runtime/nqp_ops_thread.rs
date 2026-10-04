//! The Threads family of `nqp::` ops (#11502): `currentthread`, `newthread`,
//! `threadrun`, `threadjoin`, `threadid`, `threadyield`, `threadlockcount`.
//!
//! In Rakudo these are the VM layer under `Thread`: a `Thread` keeps an
//! `MVMThread` handle in `$!vm_thread`, and `Thread.new` / `.run` / `.finish` /
//! `.id` call these ops on it. mutsu's `Thread` is native and is its own
//! handle, so each op is the matching `Thread` operation on that one object —
//! the same id allocation, the same spawn path (`spawn_thread_body`, with its
//! `clone_for_thread`), the same join. There is no second way to start a thread.
//!
//! The one visible difference: Rakudo's handle is a `BOOTThread`, while mutsu
//! hands back the `Thread` itself.

use crate::runtime::{Interpreter, RuntimeError};
use crate::symbol::Symbol;
use crate::value::{AttrMap, Value, ValueView};

/// The attributes of a `Thread` operand, or the error MoarVM raises for any
/// other handle.
fn thread_attrs(op: &str, args: &[Value]) -> Result<(Value, AttrMap), RuntimeError> {
    let handle = args.first().cloned().unwrap_or(Value::NIL);
    if let ValueView::Instance {
        class_name,
        attributes,
        ..
    } = handle.view()
        && class_name == "Thread"
    {
        let attrs = attributes.as_map().clone();
        return Ok((handle, attrs));
    }
    Err(RuntimeError::new(format!(
        "Thread handle passed to {op} must have representation MVMThread"
    )))
}

impl Interpreter {
    /// Run a Threads-family op; `None` for any other name.
    pub(crate) fn call_nqp_op_thread(
        &mut self,
        op: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        Some(match op {
            // nqp::currentthread() — the running thread's handle, the same
            // object `$*THREAD` answers.
            // Cost: O(1).
            "currentthread" => Ok(crate::runtime::current_thread_value()),
            // nqp::newthread($code, $app_lifetime) — a thread that is created
            // but not started, exactly `Thread.new(:$code, :$app_lifetime)`.
            // Cost: O(1).
            "newthread" => {
                let code = args.first().cloned().unwrap_or(Value::NIL);
                if !matches!(code.view(), ValueView::Sub(..) | ValueView::WeakSub(..)) {
                    return Some(Err(RuntimeError::new(
                        "Thread start code must be a code handle",
                    )));
                }
                let app_lifetime = args.get(1).is_some_and(|v| v.truthy());
                Ok(Self::new_thread_object(
                    Symbol::intern("Thread"),
                    code,
                    "<anon>".to_string(),
                    app_lifetime,
                ))
            }
            // nqp::threadrun($thread) — start it (`Thread.run`); answers the
            // handle.
            // Cost: O(1) plus spawning one OS thread.
            "threadrun" => thread_attrs(op, args)
                .and_then(|(handle, attrs)| self.dispatch_thread_run(&handle, &attrs)),
            // nqp::threadjoin($thread) — wait for it to finish (`Thread.finish`);
            // answers the handle.
            // Cost: O(1) plus the wait.
            "threadjoin" => thread_attrs(op, args).and_then(|(handle, attrs)| {
                self.dispatch_thread_finish(&attrs)?;
                Ok(handle)
            }),
            // nqp::threadid($thread) — its id (`Thread.id`).
            // Cost: O(1).
            "threadid" => thread_attrs(op, args)
                .map(|(_, attrs)| attrs.get("id").cloned().unwrap_or(Value::int(0))),
            // nqp::threadyield() — let another thread run; answers VMNull (Nil).
            // Cost: O(1).
            "threadyield" => {
                std::thread::yield_now();
                Ok(Value::NIL)
            }
            // nqp::threadlockcount($thread) — how many `Lock`s it holds; a
            // re-entered lock counts once.
            // Cost: O(1).
            "threadlockcount" => thread_attrs(op, args).map(|(_, attrs)| {
                let tid = attrs.get("id").and_then(|v| v.as_int()).unwrap_or(0);
                let held = crate::runtime::native_methods::thread_lock_count::lock_count_of(tid);
                Value::int(held as i64)
            }),
            _ => return None,
        })
    }
}
