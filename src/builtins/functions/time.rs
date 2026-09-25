use crate::value::{ArrayKind, RuntimeError, Value};

/// The process's own `getrusage(RUSAGE_SELF)` fields, in MoarVM's
/// `MVM_proc_getrusage` order: user seconds, user microseconds, system
/// seconds, system microseconds, then maxrss, ixrss, idrss, isrss, minflt,
/// majflt, nswap, inblock, oublock, msgsnd, msgrcv, nsignals, nvcsw, nivcsw.
/// `None` where the platform has no `getrusage` (or it failed).
///
/// The one reading of rusage, shared by `times` and `nqp::getrusage`.
///
/// Cost: O(1) + one syscall.
pub(crate) fn process_rusage() -> Option<[i64; 18]> {
    #[cfg(all(unix, feature = "native"))]
    {
        let mut usage = std::mem::MaybeUninit::<libc::rusage>::uninit();
        let ret = unsafe { libc::getrusage(libc::RUSAGE_SELF, usage.as_mut_ptr()) };
        if ret != 0 {
            return None;
        }
        let u = unsafe { usage.assume_init() };
        Some([
            widen(u.ru_utime.tv_sec),
            widen(u.ru_utime.tv_usec),
            widen(u.ru_stime.tv_sec),
            widen(u.ru_stime.tv_usec),
            widen(u.ru_maxrss),
            widen(u.ru_ixrss),
            widen(u.ru_idrss),
            widen(u.ru_isrss),
            widen(u.ru_minflt),
            widen(u.ru_majflt),
            widen(u.ru_nswap),
            widen(u.ru_inblock),
            widen(u.ru_oublock),
            widen(u.ru_msgsnd),
            widen(u.ru_msgrcv),
            widen(u.ru_nsignals),
            widen(u.ru_nvcsw),
            widen(u.ru_nivcsw),
        ])
    }
    #[cfg(not(all(unix, feature = "native")))]
    {
        None
    }
}

/// Widen an rusage field to i64. The fields' C types differ per platform
/// (`tv_usec` is an `i32` on macOS, the counters are `c_long`), so a plain
/// `as i64` is an identity cast on 64-bit Linux and a lossless widening
/// elsewhere; going through `Into` states that without either.
#[cfg(all(unix, feature = "native"))]
fn widen<T: Into<i64>>(v: T) -> i64 {
    v.into()
}

/// Perl 5-compatible `times` builtin: returns `($user, $system)` CPU times in seconds.
pub(crate) fn builtin_times() -> Result<Value, RuntimeError> {
    let (user, sys) = process_rusage().map_or((0.0, 0.0), |r| {
        (
            r[0] as f64 + r[1] as f64 / 1_000_000.0,
            r[2] as f64 + r[3] as f64 / 1_000_000.0,
        )
    });
    Ok(Value::array_with_kind(
        crate::value::Value::array_arc(vec![Value::num(user), Value::num(sys)]),
        ArrayKind::List,
    ))
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
            Ok(Value::array_with_kind(
                crate::value::Value::array_arc(vec![
                    Value::int(sec as i64),
                    Value::int(min as i64),
                    Value::int(hour as i64),
                    Value::int(mday as i64),
                    Value::int(mon as i64),
                    Value::int(year as i64),
                    Value::int(wday as i64),
                    Value::int(yday as i64),
                    Value::int(isdst as i64),
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
