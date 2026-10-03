//! The `nqp::` exception-handling ops (NQP `docs/ops.markdown`, "Exception
//! Handling"; #11497).
//!
//! MoarVM's exception object is a `BOOTException`: a bare VM object carrying a
//! message string, an arbitrary payload and a category bit set
//! (`nqp::const::CONTROL_*`, or `CATCH` = 1 for an ordinary error). mutsu
//! models it as an instance of the class name `BOOTException` with exactly
//! those three attributes, which `newexception` creates and the setters fill.
//!
//! Raising one goes through the same routines the Raku-level forms use —
//! `die`, `warn`, `take`, the loop-control signals — so a `CATCH`, `CONTROL`,
//! `try` or `$!` sees an `nqp::throw` exactly as it sees the Raku spelling.
//! In the other direction, `nqp::exception()` hands a handler the exception it
//! is handling wrapped as a `BOOTException` whose payload is the Raku
//! exception object, which is what Rakudo's own handlers receive.

use super::*;
use crate::symbol::Symbol;

/// The class name of mutsu's model of MoarVM's VM-level exception object.
const BOOT_EXCEPTION: &str = "BOOTException";

/// The category of an ordinary (non-control) exception — what MoarVM calls
/// `MVM_EX_CAT_CATCH`. It has no `nqp::const::` spelling.
const CAT_CATCH: i64 = 1;
const CONTROL_ANY: i64 = 2;
const CONTROL_NEXT: i64 = 4;
const CONTROL_REDO: i64 = 8;
const CONTROL_LAST: i64 = 16;
const CONTROL_RETURN: i64 = 32;
const CONTROL_TAKE: i64 = 128;
const CONTROL_WARN: i64 = 256;
const CONTROL_SUCCEED: i64 = 512;
const CONTROL_PROCEED: i64 = 1024;
const CONTROL_LABELED: i64 = 4096;
const CONTROL_EMIT: i64 = 16384;
const CONTROL_DONE: i64 = 32768;

/// The `nqp::const::CONTROL_*` constants, by name, for the compiler's
/// constant folding (`nqp_const_value`).
pub(crate) fn control_const_value(name: &str) -> Option<i64> {
    Some(match name {
        "CONTROL_ANY" => CONTROL_ANY,
        "CONTROL_NEXT" => CONTROL_NEXT,
        "CONTROL_REDO" => CONTROL_REDO,
        "CONTROL_LAST" => CONTROL_LAST,
        "CONTROL_RETURN" => CONTROL_RETURN,
        "CONTROL_TAKE" => CONTROL_TAKE,
        "CONTROL_WARN" => CONTROL_WARN,
        "CONTROL_SUCCEED" => CONTROL_SUCCEED,
        "CONTROL_PROCEED" => CONTROL_PROCEED,
        "CONTROL_LABELED" => CONTROL_LABELED,
        "CONTROL_AWAIT" => 8192,
        "CONTROL_EMIT" => CONTROL_EMIT,
        "CONTROL_DONE" => CONTROL_DONE,
        _ => return None,
    })
}

/// A fresh `BOOTException` with the given fields.
// Cost: O(1).
fn boot_exception(message: Value, payload: Value, category: i64) -> Value {
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("message".to_string(), message);
    attrs.insert("payload".to_string(), payload);
    attrs.insert("category".to_string(), Value::int(category));
    Value::make_instance(Symbol::intern(BOOT_EXCEPTION), attrs)
}

/// The field `key` of `ex` when it is a `BOOTException`; `None` for any other
/// value.
// Cost: O(1).
fn boot_field(ex: &Value, key: &str) -> Option<Value> {
    match ex.view() {
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if class_name == BOOT_EXCEPTION => {
            Some(attributes.as_map().get(key).cloned().unwrap_or(Value::NIL))
        }
        _ => None,
    }
}

/// The category bits MoarVM reports for a handled Raku exception object: the
/// matching `CONTROL_*` bit for a control exception, `CATCH` for an error.
// Cost: O(1).
fn category_of_raku_exception(ex: &Value) -> i64 {
    let ValueView::Instance { class_name, .. } = ex.view() else {
        return CAT_CATCH;
    };
    match class_name.resolve().as_str() {
        "CX::Next" => CONTROL_NEXT,
        "CX::Redo" => CONTROL_REDO,
        "CX::Last" => CONTROL_LAST,
        "CX::Return" => CONTROL_RETURN,
        "CX::Take" => CONTROL_TAKE,
        "CX::Warn" => CONTROL_WARN,
        "CX::Succeed" => CONTROL_SUCCEED,
        "CX::Proceed" => CONTROL_PROCEED,
        "CX::Emit" => CONTROL_EMIT,
        "CX::Done" => CONTROL_DONE,
        _ => CAT_CATCH,
    }
}

fn arg(args: &[Value], i: usize) -> Value {
    args.get(i)
        .cloned()
        .map(crate::runtime::types::unwrap_varref_value)
        .unwrap_or(Value::NIL)
}

impl Interpreter {
    /// The exception-handling `nqp::` ops. `None` means "not one of these".
    pub(crate) fn call_nqp_op_exception(
        &mut self,
        op: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        Some(match op {
            // nqp::die($msg) / nqp::die_s($msg): throw an ordinary exception
            // with that message. Rakudo hands it to a Raku handler as an
            // `X::AdHoc` whose payload is the message — exactly what `die
            // $msg` raises, so it IS `die`.
            // Cost: O(n), n = chars of the message (plus the handler search `die` pays).
            "die" | "die_s" => self.builtin_die(&[Value::str(arg(args, 0).to_string_value())]),
            // nqp::newexception(): a blank exception object for the setters
            // below to fill and `nqp::throw` to raise. Its category starts
            // at 0 and its message and payload at null.
            // Cost: O(1).
            "newexception" => Ok(boot_exception(Value::NIL, Value::NIL, 0)),
            // nqp::setmessage($ex, $msg) / setpayload($ex, $obj) /
            // setextype($ex, $cat): fill one field; each returns what it set.
            // Cost: O(1) (setmessage: O(n), n = chars of the message).
            "setmessage" | "setpayload" | "setextype" => {
                let ex = arg(args, 0);
                let value = match op {
                    "setmessage" => Value::str(arg(args, 1).to_string_value()),
                    "setpayload" => arg(args, 1),
                    _ => Value::int(crate::runtime::to_int(&arg(args, 1))),
                };
                let key = match op {
                    "setmessage" => "message",
                    "setpayload" => "payload",
                    _ => "category",
                };
                match ex.view() {
                    ValueView::Instance {
                        class_name,
                        attributes,
                        ..
                    } if class_name == BOOT_EXCEPTION => {
                        attributes.bind_attr_through(key, value.clone());
                        Ok(value)
                    }
                    _ => Err(RuntimeError::new(format!(
                        "nqp::{op} requires a VM exception object, got {}",
                        crate::value::type_name::value_type_name(&ex)
                    ))),
                }
            }
            // nqp::getextype($ex): the category bits. A Raku exception
            // object (what a handler's `$_` is) reports the category MoarVM
            // gives it: its control bit, or CATCH for an error.
            // Cost: O(1).
            "getextype" => {
                let ex = arg(args, 0);
                Ok(match boot_field(&ex, "category") {
                    Some(cat) => cat,
                    None => Value::int(category_of_raku_exception(&ex)),
                })
            }
            // nqp::getmessage($ex): the message of a VM exception object
            // (null until `setmessage`). For a Raku exception instance this
            // reuses Raku's own message rules (`exception_message_text`: a
            // user `method message` wins over the stored attribute), and
            // anything else is stringified.
            // Cost: O(n), n = chars of the message (a user `method message` runs at its own cost).
            "getmessage" => {
                let ex = arg(args, 0);
                if let Some(msg) = boot_field(&ex, "message") {
                    return Some(Ok(msg));
                }
                let msg = self
                    .exception_message_text(&ex)
                    .unwrap_or_else(|| ex.to_string_value());
                Ok(Value::str(msg))
            }
            // nqp::getpayload($ex): the payload of a VM exception object --
            // for one a handler received, the Raku exception object itself.
            // A Raku exception instance has no separate payload (null), which
            // `nqp::ifnull(nqp::getpayload($ex), $ex)` in Rakudo's core
            // `X::Wrapper` role relies on.
            // Cost: O(1).
            "getpayload" => Ok(boot_field(&arg(args, 0), "payload").unwrap_or(Value::NIL)),
            // nqp::exception(): the exception the innermost CATCH/CONTROL
            // handler running on the dynamic call stack is handling, as a VM
            // exception object; null outside any handler.
            // Cost: O(1).
            "exception" => Ok(match self.control.handled_exceptions.last() {
                Some(ex) if !ex.is_nil() => {
                    boot_exception(Value::NIL, ex.clone(), category_of_raku_exception(ex))
                }
                _ => Value::NIL,
            }),
            // nqp::throw($ex) / nqp::rethrow($ex): raise a VM exception object
            // (see `nqp_throw`). mutsu keeps a Raku exception's backtrace on
            // the object itself, so re-raising it already preserves the
            // original throw site, which is all `rethrow` adds.
            // Cost: O(1) plus the raise (`die`'s handler search, or the signal's).
            "throw" | "rethrow" => self.nqp_throw(arg(args, 0)),
            // nqp::resume($ex): resume a resumable exception from inside its
            // handler -- the same signal `.resume` raises.
            // Cost: O(1).
            "resume" => Err(RuntimeError::resume_signal()),
            // nqp::backtrace($ex): the native backtrace MoarVM captured when
            // `$ex` was thrown, as the array-of-frame-hashes `Backtrace.new`
            // expects. mutsu's `Backtrace.new` does not consume that shape --
            // it always captures the *current* call stack directly (see
            // `build_backtrace_value`) -- so an empty array is a safe,
            // non-crashing placeholder for the argument; `Backtrace.new(...)`
            // ignores it and returns backtrace of the current point of the
            // program's execution.
            // TODO: thread the exception's own captured frames through here
            // once `Backtrace.new` can be constructed from an explicit frame
            // list instead of always sampling the live stack -- see #8573.
            // Cost: O(1).
            "backtrace" => Ok(Value::array(Vec::new())),
            // nqp::backtracestrings($ex): the backtrace of a thrown exception,
            // one `   at FILE:LINE  (FILE:ROUTINE)` line per frame (` from`
            // after the first), as MoarVM renders it. An exception that was
            // never thrown has none.
            // Cost: O(f), f = frames in the backtrace.
            "backtracestrings" => Ok(Value::array(Self::nqp_backtrace_strings(&arg(args, 0)))),
            _ => return None,
        })
    }

    /// Raise a VM exception object (`nqp::throw`). A control category becomes
    /// the matching control signal; anything else is an error, raised as
    /// `die` raises one: the payload when there is one (so a Raku exception
    /// object is thrown as itself), else an `X::AdHoc` of the message.
    // Cost: O(1) plus the raise.
    fn nqp_throw(&mut self, ex: Value) -> Result<Value, RuntimeError> {
        use crate::value::Control;
        let Some(category) = boot_field(&ex, "category") else {
            // A Raku exception object: throw it as it is.
            return self.builtin_die(&[ex]);
        };
        let category = crate::runtime::to_int(&category);
        let payload = boot_field(&ex, "payload").unwrap_or(Value::NIL);
        let message = boot_field(&ex, "message").unwrap_or(Value::NIL);
        // A labeled loop control names its loop by a `Label` payload, which
        // the signals here cannot carry yet; `CONTROL_RETURN` is how MoarVM
        // unwinds to a routine's lexotic return handler, which a Raku routine
        // does not install for a thrown exception (rakudo itself ends the
        // program silently). Both stay loud errors rather than a guess.
        if category & CONTROL_LABELED != 0 {
            return Err(RuntimeError::new(
                "nqp::throw: labeled control exceptions are not supported yet",
            ));
        }
        match category {
            CONTROL_NEXT => Err(crate::runtime::loop_handler_depth::loop_control_signal(
                Control::Next,
                None,
            )),
            CONTROL_LAST => Err(crate::runtime::loop_handler_depth::loop_control_signal(
                Control::Last,
                None,
            )),
            CONTROL_REDO => Err(crate::runtime::loop_handler_depth::loop_control_signal(
                Control::Redo,
                None,
            )),
            CONTROL_TAKE => self.builtin_take_value(payload),
            CONTROL_WARN => self.builtin_warn(&[Value::str(message.to_string_value())]),
            CONTROL_SUCCEED => self.builtin_succeed(&[payload]),
            CONTROL_PROCEED => Err(RuntimeError::proceed_signal()),
            c if c & !CAT_CATCH == 0 => {
                if payload.is_nil() {
                    self.builtin_die(&[Value::str(message.to_string_value())])
                } else {
                    self.builtin_die(&[payload])
                }
            }
            c => Err(RuntimeError::new(format!(
                "nqp::throw: exception category {c} is not supported yet"
            ))),
        }
    }

    /// The `nqp::backtracestrings` lines of a thrown exception: read from the
    /// `Backtrace` its Raku exception object carries.
    // Cost: O(f), f = frames in the backtrace.
    fn nqp_backtrace_strings(ex: &Value) -> Vec<Value> {
        let raku_ex = boot_field(ex, "payload").unwrap_or_else(|| ex.clone());
        let ValueView::Instance { attributes, .. } = raku_ex.view() else {
            return Vec::new();
        };
        let Some(bt) = attributes.as_map().get("backtrace").cloned() else {
            return Vec::new();
        };
        let ValueView::Instance { attributes, .. } = bt.view() else {
            return Vec::new();
        };
        use crate::builtins::backtrace_methods::{frame_field, frames_of};
        frames_of(&attributes)
            .iter()
            .enumerate()
            .map(|(i, frame)| {
                let file = frame_field(frame, "file");
                let line = frame_field(frame, "line");
                let sub = frame_field(frame, "subname");
                let lead = if i == 0 { "   at" } else { " from" };
                Value::str(format!("{lead} {file}:{line}  ({file}:{sub})"))
            })
            .collect()
    }
}
