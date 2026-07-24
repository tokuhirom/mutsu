use crate::runtime;
use crate::value::{RuntimeError, Value, ValueView};

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
    if matches!(
        args.first().map(Value::view),
        Some(ValueView::Junction { .. })
    ) {
        return None;
    }
    let fmt = match args.first().map(Value::view) {
        Some(ValueView::Str(s)) => s.to_string(),
        _ => String::new(),
    };
    let rest: &[Value] = if args.is_empty() { &[] } else { &args[1..] };
    // Raku's `sprintf($format, *@args)` slurps its arguments, so every list-like
    // argument (Array, List/Seq/Slip, Range) flattens into one flat positional
    // list: `sprintf("%d %d", 1..2)` sees two args. `flatten_into_slurpy` performs
    // exactly the slurpy flatten, respecting itemization (a scalar-held array
    // stays one argument), and mirrors `Interpreter::builtin_sprintf`.
    let mut flattened: Vec<Value> = Vec::with_capacity(rest.len());
    runtime::types::flatten_into_slurpy(rest, &mut flattened);
    let actual_args = &flattened[..];
    // A bare type object argument needs interpreter-aware coercion: a `%s`
    // directive stringifies it to "" with rakudo's "uninitialized value of type
    // X in string context" warning (matching `~Int` / `Int.Str`) instead of
    // rendering the `(Int)` gist. Fall back to `builtin_sprintf`, which can emit
    // the warning. (Instance args already bail in `try_native_function`.)
    if actual_args
        .iter()
        .any(|a| matches!(a.view(), ValueView::Package(_)))
    {
        return None;
    }
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
