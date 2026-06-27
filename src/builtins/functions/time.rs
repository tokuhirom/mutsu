#![allow(clippy::result_large_err)]
use crate::value::{ArrayKind, RuntimeError, Value};

/// Perl 5-compatible `times` builtin: returns `($user, $system)` CPU times in seconds.
pub(crate) fn builtin_times() -> Result<Value, RuntimeError> {
    #[cfg(unix)]
    {
        let mut usage = std::mem::MaybeUninit::<libc::rusage>::uninit();
        let ret = unsafe { libc::getrusage(libc::RUSAGE_SELF, usage.as_mut_ptr()) };
        if ret == 0 {
            let usage = unsafe { usage.assume_init() };
            let user = usage.ru_utime.tv_sec as f64 + usage.ru_utime.tv_usec as f64 / 1_000_000.0;
            let sys = usage.ru_stime.tv_sec as f64 + usage.ru_stime.tv_usec as f64 / 1_000_000.0;
            Ok(Value::Array(
                crate::value::Value::array_arc(vec![Value::Num(user), Value::Num(sys)]),
                ArrayKind::List,
            ))
        } else {
            Ok(Value::Array(
                crate::value::Value::array_arc(vec![Value::Num(0.0), Value::Num(0.0)]),
                ArrayKind::List,
            ))
        }
    }
    #[cfg(not(unix))]
    {
        Ok(Value::Array(
            crate::value::Value::array_arc(vec![Value::Num(0.0), Value::Num(0.0)]),
            ArrayKind::List,
        ))
    }
}

/// Perl 5-compatible `localtime`/`gmtime` builtins.
/// With args: returns a 9-element list `($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst)`
/// Without args: returns a ctime-style formatted string.
pub(crate) fn builtin_localtime_gmtime(name: &str, args: &[Value]) -> Result<Value, RuntimeError> {
    use std::time::{SystemTime, UNIX_EPOCH};

    let epoch_secs: i64 = if args.is_empty() {
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_secs() as i64
    } else {
        args[0].to_f64() as i64
    };

    #[cfg(unix)]
    {
        let time_t = epoch_secs as libc::time_t;
        let mut tm = std::mem::MaybeUninit::<libc::tm>::uninit();
        let result = if name == "gmtime" {
            unsafe { libc::gmtime_r(&time_t, tm.as_mut_ptr()) }
        } else {
            unsafe { libc::localtime_r(&time_t, tm.as_mut_ptr()) }
        };
        if result.is_null() {
            return Err(RuntimeError::new(format!(
                "{name}: invalid time value {epoch_secs}"
            )));
        }
        let tm = unsafe { tm.assume_init() };

        let sec = tm.tm_sec;
        let min = tm.tm_min;
        let hour = tm.tm_hour;
        let mday = tm.tm_mday;
        let mon = tm.tm_mon; // 0-based
        let year = tm.tm_year + 1900;
        let wday = tm.tm_wday;
        let yday = tm.tm_yday;
        let isdst = tm.tm_isdst;

        if args.is_empty() {
            // Scalar-like context: return the ctime-style formatted string
            let dow_names = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"];
            let mon_names = [
                "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
            ];
            let dow_str = dow_names.get(wday as usize).unwrap_or(&"???");
            let mon_str = mon_names.get(mon as usize).unwrap_or(&"???");
            let scalar_str = format!(
                "{} {} {:2} {:02}:{:02}:{:02} {}",
                dow_str, mon_str, mday, hour, min, sec, year
            );
            Ok(Value::str(scalar_str))
        } else {
            // List context: return the 9-element list
            Ok(Value::Array(
                crate::value::Value::array_arc(vec![
                    Value::Int(sec as i64),
                    Value::Int(min as i64),
                    Value::Int(hour as i64),
                    Value::Int(mday as i64),
                    Value::Int(mon as i64),
                    Value::Int(year as i64),
                    Value::Int(wday as i64),
                    Value::Int(yday as i64),
                    Value::Int(isdst as i64),
                ]),
                ArrayKind::List,
            ))
        }
    }
    #[cfg(not(unix))]
    {
        let _ = (name, epoch_secs);
        Err(RuntimeError::new(
            "localtime/gmtime not supported on this platform".to_string(),
        ))
    }
}
