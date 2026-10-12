//! The scheduler-queue part of the Asynchronous `nqp::` family (#11502,
//! ADR-11502): the `ConcBlockingQueue` and `AsyncTask` REPRs, `nqp::timer` and
//! `nqp::cancel`.
//!
//! In MoarVM an async op takes a queue (a `ConcBlockingQueue`), a schedulee
//! and a handle type of REPR `AsyncTask`, and returns a cancellable handle; the
//! event later pushes the schedulee onto the queue, where whoever `shift`s it
//! (a scheduler worker, or the program itself) picks it up. mutsu models:
//!
//! - an `is repr('ConcBlockingQueue')` instance as an object whose storage is a
//!   [`SharedChannel`] under the [`QUEUE_ATTR`] attribute: `nqp::push` sends,
//!   `nqp::shift` receives (blocking, GC-quiescent), `nqp::elems` counts;
//! - an `is repr('AsyncTask')` instance as an object holding a cancellation id
//!   under [`TASK_ATTR`], from the same registry `Cancellation` uses, so
//!   `nqp::cancel` and `Cancellation.cancel` set one kind of flag;
//! - `nqp::timer` as an entry on the shared interval-timer heap whose action
//!   only sends the schedulee into the channel (cheap, no VM code on the timer
//!   thread) and stops once the flag is set;
//! - an event source that reports with arguments (`nqp::signal`) as a
//!   [`QueueSink`] registered with the source's native watcher, next to the
//!   Supply sink that watcher already serves (ADR-11502 §2.4).

use crate::runtime::native_methods::interval_timer;
use crate::runtime::native_methods::state_lock::{cancellation_state, next_cancellation_id};
use crate::runtime::{Interpreter, RuntimeError};
use crate::symbol::Symbol;
use crate::value::{SharedChannel, Value, ValueView};
use std::collections::HashMap;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};

/// The attribute that holds a `ConcBlockingQueue` instance's channel. A bare
/// name, like a Buf's `bytes`, so no declared `$!attr` can collide with it.
const QUEUE_ATTR: &str = "queue";
/// The attribute that holds an `AsyncTask` instance's cancellation id.
const TASK_ATTR: &str = "task";

/// An operand with any argument wrapper and container stripped.
fn operand(v: &Value) -> Value {
    crate::runtime::types::unwrap_varref_value(v.clone()).deref_container()
}

/// The channel of a `ConcBlockingQueue` instance.
// Cost: O(1).
fn queue_of(v: &Value) -> Option<(Symbol, SharedChannel)> {
    let v = operand(v);
    let ValueView::Instance {
        class_name,
        attributes,
        ..
    } = v.view()
    else {
        return None;
    };
    let channel = attributes.as_map().get(QUEUE_ATTR).cloned()?;
    match channel.view() {
        ValueView::Channel(c) => Some((class_name, (*c).clone())),
        _ => None,
    }
}

/// The cancellation id of an `AsyncTask` instance.
// Cost: O(1).
fn task_id_of(v: &Value) -> Option<u64> {
    match operand(v).view() {
        ValueView::Instance { attributes, .. } => match attributes.as_map().get(TASK_ATTR)?.view() {
            ValueView::Int(id) if id > 0 => Some(id as u64),
            _ => None,
        },
        _ => None,
    }
}

/// The queue end of an async task (ADR-11502 §2.4): where a native event
/// source pushes `[schedulee, args...]` for an `nqp::` async op, until the
/// task is cancelled. Built only on the op's own thread; the source's thread
/// only calls [`QueueSink::push`].
pub(crate) struct QueueSink {
    queue: SharedChannel,
    schedulee: Value,
    cancelled: Arc<AtomicBool>,
}

impl QueueSink {
    /// Whether the task was cancelled (`nqp::cancel`): the source drops the
    /// sink at its next sweep.
    // Cost: O(1).
    pub(crate) fn is_cancelled(&self) -> bool {
        self.cancelled.load(Ordering::Acquire)
    }

    /// Push a raw `[schedulee, args...]` array, unless the task is cancelled.
    // Cost: O(a), a = args.len().
    pub(crate) fn push(&self, args: Vec<Value>) {
        if self.is_cancelled() {
            return;
        }
        let mut items = Vec::with_capacity(args.len() + 1);
        items.push(self.schedulee.clone());
        items.extend(args);
        self.queue.send(Value::vm_array(items));
    }
}

/// The handle class of an async op's last operand, checked to be of REPR
/// `AsyncTask` (`<op> result type must have REPR AsyncTask` otherwise).
// Cost: O(1).
fn handle_class(interp: &Interpreter, op: &str, ty: &Value) -> Result<Symbol, RuntimeError> {
    match operand(ty).view() {
        ValueView::Package(name) if interp.declared_class_repr(name.as_str()) == Some("AsyncTask") => {
            Ok(name)
        }
        _ => Err(RuntimeError::new(format!(
            "{op} result type must have REPR AsyncTask"
        ))),
    }
}

/// The channel of an async op's queue operand (`<op> <what> must have
/// ConcBlockingQueue REPR` otherwise).
// Cost: O(1).
pub(crate) fn queue_operand(op: &str, what: &str, v: &Value) -> Result<SharedChannel, RuntimeError> {
    queue_of(v).map(|(_, q)| q).ok_or_else(|| {
        RuntimeError::new(format!("{op} {what} must have ConcBlockingQueue REPR"))
    })
}

/// Rakudo checks an async op's operand count at compile time.
// Cost: O(1).
fn check_arity(op: &str, args: &[Value], want: usize) -> Result<(), RuntimeError> {
    if args.len() == want {
        return Ok(());
    }
    Err(RuntimeError::new(format!(
        "Arg count {} doesn't equal required operand count {want} for op '{op}'",
        args.len()
    )))
}

impl Interpreter {
    /// A new instance of a class of one of the two REPRs, with its storage.
    // Cost: O(1).
    pub(crate) fn create_async_repr_instance(&mut self, class: Symbol, repr: &str) -> Value {
        let mut attrs = HashMap::new();
        if repr == "ConcBlockingQueue" {
            attrs.insert(
                QUEUE_ATTR.to_string(),
                Value::channel(SharedChannel::new()),
            );
        } else {
            attrs.insert(TASK_ATTR.to_string(), Value::int(next_cancellation_id() as i64));
        }
        Value::make_instance(class, attrs)
    }

    /// The positional ops on a `ConcBlockingQueue` instance, ahead of the
    /// REPR check that would refuse the object. `None` when `args[0]` is not
    /// a queue or the op is not a positional one.
    pub(crate) fn nqp_queue_op(
        &mut self,
        op: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if !matches!(
            op,
            "push" | "shift" | "elems" | "pop" | "unshift" | "atpos" | "bindpos" | "existspos"
        ) {
            return None;
        }
        let (class, queue) = queue_of(args.first()?)?;
        Some(match op {
            // nqp::push($q, $v): enqueue `$v` and answer it.
            // Cost: O(1) (one lock and a wakeup).
            "push" => {
                let value = args.get(1).cloned().unwrap_or(Value::NIL);
                queue.send(value.clone());
                Ok(value)
            }
            // nqp::shift($q): the oldest item, blocking until there is one.
            // Cost: O(1) once an item is there; blocks quiescently until then.
            "shift" => queue
                .receive_result()
                .map_err(|e| RuntimeError::new(e.to_string_value())),
            // nqp::elems($q): the items waiting.
            // Cost: O(1).
            "elems" => Ok(Value::int(queue.len() as i64)),
            // Cost: O(1).
            _ => Err(RuntimeError::new(format!(
                "This representation (ConcBlockingQueue) does not support positional access (for type {})",
                crate::value::user_facing_type_name(class.as_str())
            ))),
        })
    }

    /// `nqp::timer` / `nqp::cancel` / `nqp::signal` / `nqp::watchfile`.
    pub(crate) fn call_nqp_op_async(
        &mut self,
        op: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        Some(match op {
            // nqp::timer($queue, $schedulee, $timeout_ms, $repeat_ms, $type):
            // push `$schedulee` onto `$queue` after `$timeout_ms`, then every
            // `$repeat_ms` when that is non-zero; answer a new `$type` handle.
            // Cost: O(log t), t = pending timer entries (the heap insert).
            "timer" => self.nqp_timer(args),
            // nqp::cancel($handle): stop the task's further events; answer the
            // handle.
            // Cost: O(1).
            "cancel" => {
                if let Some(flag) = args.first().and_then(task_id_of).and_then(cancellation_state)
                {
                    flag.store(true, Ordering::Release);
                    // A cancelled `nqp::signal` gives the signal back to its
                    // previous disposition now, as MoarVM does, not at the
                    // next delivery.
                    crate::runtime::signal_watcher::sweep_retired_registrations();
                }
                Ok(args.first().cloned().unwrap_or(Value::NIL))
            }
            // nqp::signal($setup_queue, $setup_schedulee, $queue, $schedulee,
            //             $signum, $type):
            // on each delivery of `$signum`, push `[$schedulee, $signum]` onto
            // `$queue`; push `[$setup_schedulee]` onto `$setup_queue` once the
            // handler is installed; answer a new `$type` handle.
            // Cost: O(w + s), w = live signal watchers (the retired-watcher
            // sweep), s = signal numbers watched.
            "signal" => self.nqp_signal(args),
            // nqp::watchfile($queue, $schedulee, $path, $type): push
            // `[$schedulee, $name, $renamed, $error]` onto `$queue` for each
            // change to `$path` (a file, or a directory's entries); answer a
            // new `$type` handle.
            // Cost: O(1) here; the watcher thread pays O(n) per poll, n =
            // entries of the watched directory (1 for a file).
            "watchfile" => self.nqp_watchfile(args),
            _ => return None,
        })
    }

    /// A new handle of `class` with its cancellation flag.
    // Cost: O(1).
    fn new_task(&mut self, class: Symbol) -> Result<(Value, Arc<AtomicBool>), RuntimeError> {
        let handle = self.create_async_repr_instance(class, "AsyncTask");
        let flag = task_id_of(&handle)
            .and_then(cancellation_state)
            .ok_or_else(|| RuntimeError::new("async task: no cancellation state"))?;
        Ok((handle, flag))
    }

    fn nqp_signal(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        check_arity("signal", args, 6)?;
        let class = handle_class(self, "signal", &args[5])?;
        let queue = queue_operand("signal", "target queue", &args[2])?;
        let setup_queue = queue_operand("signal", "setup notify queue", &args[0])?;
        let signum = crate::runtime::to_int(&operand(&args[4]));
        // MoarVM handles the standard signals (its `valid_sigs` mask), not the
        // real-time ones.
        if !(1..32).contains(&signum) {
            return Err(RuntimeError::new(format!(
                "Unsupported signal handler {signum}"
            )));
        }
        let (handle, cancelled) = self.new_task(class)?;
        let sink = QueueSink {
            queue,
            schedulee: args[3].clone(),
            cancelled,
        };
        crate::runtime::signal_watcher::register_signal_queue(signum as i32, sink)
            .map_err(crate::runtime::builtins_system::refused_thread_error)?;
        setup_queue.send(Value::vm_array(vec![args[1].clone()]));
        Ok(handle)
    }

    fn nqp_watchfile(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        check_arity("watchfile", args, 4)?;
        let class = handle_class(self, "file watch", &args[3])?;
        let queue = queue_operand("file watch", "target queue", &args[0])?;
        let path = operand(&args[2]).to_string_value();
        let (handle, cancelled) = self.new_task(class)?;
        let sink = QueueSink {
            queue,
            schedulee: args[1].clone(),
            cancelled,
        };
        self.watch_path_queue(&path, sink)?;
        Ok(handle)
    }

    fn nqp_timer(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        check_arity("timer", args, 5)?;
        let handle_class = handle_class(self, "timer", &args[4])?;
        let queue = queue_operand("timer", "target queue", &args[0])?;
        let ms = |v: &Value| crate::runtime::to_int(&operand(v)).max(0) as f64 / 1000.0;
        let timeout = interval_timer::clamp_delay_secs(ms(&args[2]));
        let repeat = ms(&args[3]);
        let repeat = (repeat > 0.0).then(|| interval_timer::clamp_delay_secs(repeat));
        let (handle, flag) = self.new_task(handle_class)?;
        let schedulee = args[1].clone();
        interval_timer::register_entry(
            timeout,
            Box::new(move || {
                if flag.load(Ordering::Acquire) {
                    return None;
                }
                queue.send(schedulee.clone());
                repeat
            }),
        )
        .map_err(crate::runtime::builtins_system::refused_thread_error)?;
        Ok(handle)
    }
}
