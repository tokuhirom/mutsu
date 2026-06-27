#![allow(clippy::result_large_err)]

mod dispatch_0arg;
mod dispatch_1arg;
mod dispatch_2arg;
mod dispatch_3arg;
mod dispatch_variadic;
mod flat;
mod junction;
mod math;
mod sprintf_fmt;
mod time;
mod uniparse;

use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value};
use dispatch_0arg::native_function_0arg;
use dispatch_1arg::native_function_1arg;
use dispatch_2arg::native_function_2arg;
use dispatch_3arg::native_function_3arg;
use dispatch_variadic::native_function_variadic;
use sprintf_fmt::native_sprintf;
use time::builtin_localtime_gmtime;

pub(crate) use flat::{deitemize_flat_operand, flat_val, join_flat};
pub(crate) use junction::build_junction;
pub(crate) use math::factorial_bigint;
pub(crate) use uniparse::uniparse_impl;

// ── Built-in function dispatch ───────────────────────────────────────
/// Try to dispatch a built-in function call.
pub(crate) fn native_function(
    name_sym: Symbol,
    args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    let name = name_sym.resolve();
    let name = name.as_str();
    // Junction constructors are pure value assembly (no autothreading: the
    // constructor wraps its arguments). Route every arity here.
    if matches!(name, "any" | "all" | "one" | "none") {
        return Some(Ok(build_junction(name, args.to_vec())));
    }
    // Always-variadic functions: route regardless of arity.
    // zip:with needs interpreter access (for calling the combiner), so return None
    // when a :with Pair is present to fall through to the interpreter.
    if name == "sum" {
        return native_function_variadic(name, args);
    }
    // chrs(*@codes) is variadic; route every arity through the variadic arm so
    // chrs(72,105) etc. are reachable (not just the 4+ arg path).
    if name == "chrs" {
        return native_function_variadic(name, args);
    }
    // pack/unpack take a template plus a variable item list; route every arity.
    if name == "pack" || name == "unpack" {
        return native_function_variadic(name, args);
    }
    if name == "zip" {
        if args
            .iter()
            .any(|a| matches!(a, Value::Pair(k, _) if k == "with"))
        {
            return None;
        }
        return native_function_variadic(name, args);
    }
    if name == "split" {
        return crate::builtins::split::native_split_function(args);
    }
    if name == "sprintf" || name == "zprintf" {
        return native_sprintf(args, name == "zprintf");
    }
    if name == "localtime" || name == "gmtime" {
        return Some(builtin_localtime_gmtime(name, args));
    }
    match args.len() {
        0 => native_function_0arg(name),
        1 => native_function_1arg(name, &args[0]),
        2 => native_function_2arg(name, &args[0], &args[1]),
        3 => native_function_3arg(name, &args[0], &args[1], &args[2]),
        _ => native_function_variadic(name, args),
    }
}
