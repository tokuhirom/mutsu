use crate::value::{RuntimeError, Value, ValueView};

pub(crate) fn native_function_3arg(
    name: &str,
    arg1: &Value,
    arg2: &Value,
    arg3: &Value,
) -> Option<Result<Value, RuntimeError>> {
    match name {
        "expmod" => Some(crate::builtins::expmod(arg1, arg2, arg3)),
        // `unimatch($x, $value, $prop)` is `$x.unimatch($value, $prop)` -- one
        // implementation.
        // Cost: O(1) (a table lookup) plus the value name's parse.
        "unimatch" => crate::builtins::method_table::scalars::unicode::unimatch_in(
            arg1,
            &[arg2.clone(), arg3.clone()],
        ),
        // Cost: O(k) amortized, k = chars returned (see `native_substr_slice`).
        "substr" => {
            if matches!(arg1.view(), ValueView::Junction { .. })
                || matches!(arg2.view(), ValueView::Junction { .. })
                || matches!(arg3.view(), ValueView::Junction { .. })
            {
                return None;
            }
            crate::builtins::substr::native_substr_slice(arg1, arg2, Some(arg3))
        }
        _ => None,
    }
}
