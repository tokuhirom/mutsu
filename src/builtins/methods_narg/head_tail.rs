use crate::value::{RuntimeError, Value};

/// The native cascade delegates the counted forms to the same handlers as
/// the `Any.head` and `Any.tail` rows (ADR-11276).
pub(super) fn dispatch(
    target: &Value,
    method: &str,
    arg: &Value,
) -> Option<Result<Value, RuntimeError>> {
    match method {
        "head" => crate::builtins::method_table::list::head(target, std::slice::from_ref(arg)),
        "tail" => crate::builtins::method_table::list::tail(target, std::slice::from_ref(arg)),
        _ => None,
    }
}
