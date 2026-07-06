use super::time::builtin_times;
use crate::builtins::rng::{builtin_rand, builtin_srand_auto};
use crate::value::{RuntimeError, Value};

pub(crate) fn native_function_0arg(name: &str) -> Option<Result<Value, RuntimeError>> {
    match name {
        "rand" => Some(Ok(Value::num(builtin_rand()))),
        "now" => Some(Ok(Value::make_instant_now())),
        "time" => {
            let secs = crate::value::current_time_secs_f64() as i64;
            Some(Ok(Value::int(secs)))
        }
        "srand" => {
            builtin_srand_auto();
            Some(Ok(Value::NIL))
        }
        "times" => Some(builtin_times()),
        _ => None,
    }
}
