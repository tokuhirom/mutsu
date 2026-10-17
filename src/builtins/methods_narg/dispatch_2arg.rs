use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value};

pub(crate) fn native_method_2arg(
    target: &Value,
    method_sym: Symbol,
    arg1: &Value,
    arg2: &Value,
) -> Option<Result<Value, RuntimeError>> {
    let method = method_sym.resolve();
    let method = method.as_str();

    // Scalar containers are transparent for method dispatch (no .VAR at this arity).
    let target = target.descalarize();
    // `Backtrace`: the rows' handlers (ADR-11276 §9.35).
    if let Some(answer) = crate::builtins::method_table::backtrace::answer(
        target,
        method,
        &[arg1.clone(), arg2.clone()],
    ) {
        return Some(answer);
    }
    // `fmt`, `substr` and `base` are rows that take any plain argument, and
    // `unimatch` the `Cool` row (ADR-11276 §9.113); `in-range`, `substr-eq`,
    // `split`, `comb` and `naive-word-wrapper` are rows too.
    None
}
