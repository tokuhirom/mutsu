#![allow(clippy::result_large_err)]
use crate::runtime;
use crate::value::{RuntimeError, Value};

/// Native (interpreter-free) `sprintf` / `zprintf`.
///
/// Pure: consults only the format string and argument values via the free
/// functions in `runtime::sprintf`, so it runs entirely in the VM instead of
/// falling back to `Interpreter::call_function`. Mirrors
/// `Interpreter::builtin_sprintf` exactly, including the single-Array-argument
/// flattening rule (`sprintf("%d", [42])` treats array elements as args).
///
/// Returns `None` (falling back to the interpreter) when the format argument is
/// a Junction: that case threads `.Str` per eigenvalue, which needs interpreter
/// value rendering. `try_native_function` has already excluded `Instance` args,
/// so object stringification likewise still routes through the interpreter.
pub(crate) fn native_sprintf(args: &[Value], z_mode: bool) -> Option<Result<Value, RuntimeError>> {
    if matches!(args.first(), Some(Value::Junction { .. })) {
        return None;
    }
    let fmt = match args.first() {
        Some(Value::Str(s)) => s.to_string(),
        _ => String::new(),
    };
    let rest: &[Value] = if args.is_empty() { &[] } else { &args[1..] };
    // Raku: sprintf("%d", [42]) treats the single array's elements as args.
    let flattened: Vec<Value>;
    let actual_args = if rest.len() == 1 {
        if let Value::Array(items, ..) = &rest[0] {
            flattened = items.as_ref().clone().items;
            &flattened[..]
        } else {
            rest
        }
    } else {
        rest
    };
    if let Err(e) = runtime::sprintf::validate_sprintf_directives(&fmt, actual_args.len()) {
        return Some(Err(e));
    }
    if let Err(e) = runtime::sprintf::validate_sprintf_arg_types(&fmt, actual_args) {
        return Some(Err(e));
    }
    let rendered = if z_mode {
        runtime::sprintf::format_zprintf_args(&fmt, actual_args)
    } else {
        runtime::sprintf::format_sprintf_args(&fmt, actual_args)
    };
    Some(Ok(Value::str(rendered)))
}
