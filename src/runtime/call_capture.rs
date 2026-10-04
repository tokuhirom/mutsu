//! A call's argument capture, for `nqp::usecapture` / `nqp::savecapture`
//! (#11496), and the `nqp::capture*` ops that read one.
//!
//! MoarVM keeps every frame's incoming callsite and arguments, and
//! `usecapture` hands them out as an `MVMCapture`. mutsu binds arguments
//! straight into the callee's variables and keeps no such record, so the
//! record is made only for code that asks: the compiler flags a code object
//! that reads `usecapture`/`savecapture` (`CompiledCode::uses_capture`), the
//! frameless fast/light call paths decline it, and each remaining entry —
//! the named-sub, closure and method binders — stores the raw arguments as
//! a `Capture` on the call's VM frame (`VmCallFrame::call_capture`). The
//! frame, not the env, because a return merge copies callee env writes
//! back into the caller.
//! Positional and named arguments keep their call-site shape (a slip is
//! already flattened by the caller, a slurpy parameter has not yet gathered
//! anything), and a method's invocant is the first positional, as in MoarVM.
//!
//! A code object that does not read the capture pays nothing.

use crate::runtime::types::unwrap_varref_value;
use crate::runtime::{Interpreter, RuntimeError};
use crate::value::{Value, ValueMap, ValueView};

/// The capture of a call with `args` (and `invocant`, for a method).
// Cost: O(a), a = arguments.
pub(crate) fn call_capture_value(invocant: Option<&Value>, args: &[Value]) -> Value {
    let mut positional: Vec<Value> = invocant.cloned().into_iter().collect();
    let mut named = ValueMap::default();
    for raw in args {
        let arg = unwrap_varref_value(raw.clone());
        if crate::runtime::types::is_internal_named_arg(&arg) {
            continue;
        }
        match arg.view() {
            ValueView::Pair(key, value) => {
                named.insert(key.to_string(), value.clone());
            }
            _ => positional.push(arg),
        }
    }
    Value::capture(positional, named)
}

impl Interpreter {
    /// Record this call's capture for a code object that reads it. Called by
    /// each binder once the callee's own env scope is in place.
    // Cost: O(1) when `uses_capture` is false, else O(a), a = arguments.
    pub(crate) fn record_call_capture(
        &mut self,
        uses_capture: bool,
        invocant: Option<&Value>,
        args: &[Value],
    ) {
        if uses_capture && let Some(frame) = self.call_frames.last_mut() {
            frame.call_capture = Some(call_capture_value(invocant, args));
        }
    }

    /// The capture `nqp::usecapture` answers: the innermost recorded call's
    /// (frames an inline construct pushes record none and are passed over),
    /// or an empty one in the mainline (whose MAIN call has no arguments).
    // Cost: O(f), f = call frames above the recorded one (usually 0).
    pub(crate) fn current_call_capture(&self) -> Value {
        self.call_frames
            .iter()
            .rev()
            .find_map(|frame| frame.call_capture.clone())
            .unwrap_or_else(|| Value::capture(Vec::new(), ValueMap::default()))
    }

    /// Try a capture `nqp::` op. An op this table does not know goes on to
    /// the file-handle / filesystem table.
    pub(crate) fn call_nqp_op_capture(
        &mut self,
        op: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let want = match op {
            "usecapture" | "savecapture" => 0,
            "captureposelems" | "capturehasnameds" | "capturenamedshash" => 1,
            "captureposarg" | "captureposarg_i" | "captureposarg_n" | "captureposarg_s"
            | "captureposprimspec" | "captureexistsnamed" => 2,
            _ => return self.call_nqp_op_fs(op, args),
        };
        if args.len() != want {
            return Some(Err(RuntimeError::new(format!(
                "Arg count {} doesn't equal required operand count {want} for op '{op}'",
                args.len()
            ))));
        }
        if want == 0 {
            // nqp::usecapture() / nqp::savecapture(): the current call's
            // capture. A `Capture` is immutable, so the copy `savecapture`
            // makes to outlive the frame is the same value.
            // Cost: O(1).
            return Some(Ok(self.current_call_capture()));
        }
        let capture = unwrap_varref_value(args[0].clone());
        let ValueView::Capture { positional, named } = capture.view() else {
            return Some(Err(RuntimeError::new(
                "Capture operation requires an MVMCapture",
            )));
        };
        let index = || args.get(1).map(crate::runtime::to_int).unwrap_or(0);
        let positional_at = |i: i64| -> Result<Value, RuntimeError> {
            usize::try_from(i)
                .ok()
                .and_then(|i| positional.get(i).cloned())
                .ok_or_else(|| {
                    RuntimeError::new(format!(
                        "Capture argument index ({i}) out of range (0..^{}) for {op}",
                        positional.len()
                    ))
                })
        };
        Some(match op {
            // Cost: O(1).
            "captureposelems" => Ok(Value::int(positional.len() as i64)),
            // Cost: O(1).
            "capturehasnameds" => Ok(Value::int(i64::from(!named.is_empty()))),
            // nqp::captureexistsnamed($c, $name): 1 when the named argument
            // was passed.
            // Cost: O(m), m = chars of $name.
            "captureexistsnamed" => {
                let name = args[1].to_string_value();
                Ok(Value::int(i64::from(named.contains_key(&name))))
            }
            // nqp::capturenamedshash($c): the named arguments as a hash.
            // Cost: O(k), k = named arguments.
            "capturenamedshash" => Ok(Value::hash(named.clone())),
            // Cost: O(1).
            "captureposarg" => positional_at(index()),
            // The native readers want a native argument; mutsu passes every
            // argument as an object (see `captureposprimspec`), so each is
            // MoarVM's error for an object argument.
            // Cost: O(1).
            "captureposarg_i" | "captureposarg_n" | "captureposarg_s" => positional_at(index())
                .and_then(|_| {
                    let kind = match op {
                        "captureposarg_i" => "an integer",
                        "captureposarg_n" => "a number",
                        _ => "a string",
                    };
                    Err(RuntimeError::new(format!(
                        "Capture argument is not {kind} argument for {op}"
                    )))
                }),
            // nqp::captureposprimspec($c, $i): the native kind of an
            // argument; 0 (an object), since mutsu passes no native
            // arguments.
            // Cost: O(1).
            "captureposprimspec" => positional_at(index()).map(|_| Value::int(0)),
            _ => return None,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn named_and_positional_arguments_keep_their_shape() {
        let args = vec![
            Value::int(1),
            Value::pair("k".to_string(), Value::int(5)),
            Value::str_from("b"),
        ];
        let cap = call_capture_value(Some(&Value::str_from("self")), &args);
        let ValueView::Capture { positional, named } = cap.view() else {
            panic!("not a capture");
        };
        assert_eq!(positional.len(), 3);
        assert_eq!(named.len(), 1);
    }
}
