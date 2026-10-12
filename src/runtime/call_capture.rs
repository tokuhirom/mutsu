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
//! A positional argument keeps the caller's container, as MoarVM's does
//! (#12839): the entry *requests* the record before binding
//! ([`CallCapture::Pending`]) and the binder, which alone knows each
//! argument's caller source, fills it through the `|c` parameter's routine
//! (`capture_param_containers`). An argument read from a caller scalar becomes
//! a shared cell written back at return; an element argument (`f(@a[0])`)
//! arrives as the element's own cell, because the `IndexArgRef` gate counts a
//! capture-reading callee as one that binds every argument's container. The
//! capture is also bound under [`crate::symbol::wk::call_capture`] in the
//! callee env, which is where the return writeback reads each cell's final
//! value. A path that binds without the general binder (an empty signature's
//! short cut) still records the plain values.
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

/// A call frame's capture record.
pub(crate) enum CallCapture {
    /// Requested by the call entry; the binder has not filled it yet. Holds a
    /// method's invocant, the capture's first positional.
    Pending { invocant: Option<Value> },
    /// The capture `nqp::usecapture` answers.
    Recorded(Value),
}

impl Interpreter {
    /// Ask the binder to record this call's capture, for a code object that
    /// reads it. Called by each call entry once the callee's frame is pushed
    /// and before its arguments are bound.
    // Cost: O(1).
    pub(crate) fn request_call_capture(&mut self, uses_capture: bool, invocant: Option<&Value>) {
        if uses_capture && let Some(frame) = self.call_frames.last_mut() {
            frame.call_capture = Some(CallCapture::Pending {
                invocant: invocant.cloned(),
            });
        }
    }

    /// Complete a requested record the binder did not fill (a call bound
    /// without the general binder): the arguments as plain values.
    // Cost: O(1) when nothing is pending, else O(a), a = arguments.
    pub(crate) fn finish_call_capture(&mut self, args: &[Value]) {
        if let Some(frame) = self.call_frames.last_mut()
            && let Some(CallCapture::Pending { invocant }) = &frame.call_capture
        {
            let capture = call_capture_value(invocant.as_ref(), args);
            frame.call_capture = Some(CallCapture::Recorded(capture));
        }
    }

    /// The binder's half: when the running frame requested a record, build it
    /// from the call's raw `args` (index-parallel to `arg_sources`, the
    /// caller's variable names), keeping each positional's caller container,
    /// and push the writebacks for the cells it made onto `rw_bindings`.
    // Cost: O(1) when nothing is pending, else O(a), a = arguments.
    pub(crate) fn fill_call_capture(
        &mut self,
        args: &[Value],
        arg_sources: Option<&[Option<String>]>,
        rw_bindings: &mut Vec<(String, String)>,
    ) {
        let Some(frame) = self.call_frames.last_mut() else {
            return;
        };
        let Some(CallCapture::Pending { invocant }) = &mut frame.call_capture else {
            return;
        };
        let mut positional: Vec<Value> = invocant.take().into_iter().collect();
        let mut named = ValueMap::default();
        let key = crate::symbol::wk::call_capture();
        let key_str = key.resolve();
        for (arg_idx, raw) in args.iter().enumerate() {
            let arg = unwrap_varref_value(raw.clone());
            if crate::runtime::types::is_internal_named_arg(&arg) {
                continue;
            }
            match arg.view() {
                ValueView::Pair(name, value) => {
                    named.insert(name.to_string(), value.clone());
                }
                _ => {
                    let source = arg_sources
                        .and_then(|names| names.get(arg_idx))
                        .and_then(Option::as_ref);
                    let elem_idx = positional.len();
                    positional.push(self.capture_positional_container(
                        raw,
                        source,
                        Some(&key_str),
                        elem_idx,
                        rw_bindings,
                        // Recorded before any parameter is bound, so a
                        // same-named parameter replaces the cell afterwards.
                        &[],
                    ));
                }
            }
        }
        let capture = Value::capture(positional, named);
        self.env.insert_sym(key, capture.clone());
        if let Some(frame) = self.call_frames.last_mut() {
            frame.call_capture = Some(CallCapture::Recorded(capture));
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
            .find_map(|frame| match &frame.call_capture {
                Some(CallCapture::Recorded(capture)) => Some(capture.clone()),
                _ => None,
            })
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
            "capturenamedshash" => Ok(Value::vm_hash(named.clone())),
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
