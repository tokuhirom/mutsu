use crate::builtins::rng::{builtin_rand, builtin_srand_auto};
use crate::value::{RuntimeError, Value};

/// Wall clock in nanoseconds since the Unix epoch, saturating rather than
/// wrapping (an `i64` of nanoseconds runs out in the year 2262). Shared by the
/// `nano` term and `nqp::time`.
// Cost: O(1).
pub(crate) fn epoch_nanos() -> i64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| i64::try_from(d.as_nanos()).unwrap_or(i64::MAX))
        .unwrap_or(0)
}

pub(crate) fn native_function_0arg(name: &str) -> Option<Result<Value, RuntimeError>> {
    match name {
        "rand" => Some(Ok(Value::num(builtin_rand()))),
        "now" => Some(Ok(Value::make_instant_now())),
        "time" => {
            let secs = crate::value::current_time_secs_f64() as i64;
            Some(Ok(Value::int(secs)))
        }
        // Cost: O(1).
        "nano" => Some(Ok(Value::int(epoch_nanos()))),
        "srand" => {
            builtin_srand_auto();
            Some(Ok(Value::NIL))
        }
        _ => None,
    }
}
