//! The process, system-introspection and time `nqp::` ops (#11501):
//! `getpid`, `exit`, `uname`, `cpucores`, `getenvhash`, `sleep`, ...
//!
//! Each shares its routine with the Raku-level API Rakudo builds on it:
//! `Kernel` (`sys_resources`, `signal_table`, `io_sysinfo_host`), `$*VM.config`
//! (`io_sysinfo_vm_config`), `%*ENV`, `exit` and `sleep`.

use crate::runtime::{Interpreter, RuntimeError};
use crate::value::Value;

/// `nqp::const::UNAME_*`: the indices of `nqp::uname`'s result.
pub(crate) fn uname_const_value(name: &str) -> Option<i64> {
    Some(match name {
        "UNAME_SYSNAME" => 0,
        "UNAME_RELEASE" => 1,
        "UNAME_VERSION" => 2,
        "UNAME_MACHINE" => 3,
        _ => return None,
    })
}

impl Interpreter {
    /// Try a process / system / time `nqp::` op. `None` means "not an op this
    /// table knows"; the caller then tries the filesystem table.
    pub(crate) fn call_nqp_op_sys(
        &mut self,
        op: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        Some(match op {
            // -- processes --
            // Cost: O(1).
            "getpid" => Ok(Value::int(crate::runtime::current_process_id())),
            // Cost: O(1).
            "getppid" => Ok(Value::int(parent_process_id())),
            // nqp::execname(): the path of the running interpreter binary
            // (`$*EXECUTABLE`).
            // Cost: O(p), p = length of the path (cached).
            "execname" => Ok(Value::str_from(Self::cached_executable_path_string())),
            // nqp::exit($code): end the process with `$code`. Unlike Raku's
            // `exit`, MoarVM's op neither calls `&*EXIT` nor runs END phasers
            // (measured against rakudo); buffered output is still flushed.
            // Cost: O(1) plus the shutdown.
            "exit" => {
                let code = args.first().map(crate::runtime::to_int).unwrap_or(0);
                self.control.skip_end_phasers = true;
                self.request_process_exit(code);
                Ok(Value::NIL)
            }

            // -- system introspection --
            // Cost: O(1) plus one syscall.
            "cpucores" => Ok(Value::int(crate::runtime::sys_resources::cpu_core_count())),
            // Cost: O(f), f = size of /proc/meminfo.
            "freemem" => Ok(Value::int(crate::runtime::sys_resources::free_memory())),
            // Cost: O(f), f = size of /proc/meminfo.
            "totalmem" => Ok(Value::int(crate::runtime::sys_resources::total_memory())),
            // nqp::uname(): sysname, release, version and machine, indexed by
            // `nqp::const::UNAME_*`.
            // Cost: O(1) (the host probe is cached).
            "uname" => {
                let host = crate::runtime::io_sysinfo_host::host_info();
                Ok(Value::array(vec![
                    Value::str(host.sysname.clone()),
                    Value::str(host.release.clone()),
                    Value::str(host.version.clone()),
                    Value::str(host.machine.clone()),
                ]))
            }
            // nqp::getsignals(): name, number, name, number, ... for every
            // signal MoarVM knows (0 where this platform lacks it).
            // Cost: O(s), s = signals listed (a constant 35).
            "getsignals" => Ok(Value::array(
                crate::runtime::signal_table::SIGNALS
                    .iter()
                    .flat_map(|(name, num)| [Value::str_from(name), Value::int(*num)])
                    .collect(),
            )),
            // nqp::getenvhash(): a fresh hash of the process environment, the
            // map `%*ENV` starts from.
            // Cost: O(e), e = total size of the environment.
            "getenvhash" => Ok(Value::hash(crate::runtime::runtime_init::os_env_hash())),
            // nqp::backendconfig(): the backend's configuration hash, the one
            // `$*VM.config` answers. mutsu reports its own build facts there,
            // not MoarVM's.
            // Cost: O(k), k = keys in the config.
            "backendconfig" => Ok(Value::hash(
                crate::runtime::io_sysinfo_vm_config::vm_config(),
            )),

            // -- timish --
            // nqp::sleep($seconds): block for that long; answers the
            // seconds as a Num. Raku's `sleep` (wakes on a cross-thread exit).
            // Cost: O(1) plus the sleep.
            "sleep" => {
                let secs = args.first().map(|v| v.to_f64()).unwrap_or(0.0);
                self.builtin_sleep(&[Value::num(secs)])
                    .map(|_| Value::num(secs))
            }
            // nqp::decodelocaltime($epoch): the epoch broken down in the local
            // zone (sec, min, hour, mday, month, year, wday, yday, isdst).
            // Cost: O(1) plus one `localtime_r(3)`.
            "decodelocaltime" => {
                let epoch = args.first().map(crate::runtime::to_int).unwrap_or(0);
                Ok(Value::array(
                    crate::runtime::sys_resources::decode_local_time(epoch)
                        .into_iter()
                        .map(Value::int)
                        .collect(),
                ))
            }
            _ => return self.call_nqp_op_fs(op, args),
        })
    }
}

/// The parent process's id; 0 where there is no process to ask (wasm).
fn parent_process_id() -> i64 {
    #[cfg(all(unix, feature = "native"))]
    {
        // SAFETY: `getppid` takes no arguments, touches no memory and cannot
        // fail.
        i64::from(unsafe { libc::getppid() })
    }
    #[cfg(not(all(unix, feature = "native")))]
    {
        0
    }
}
