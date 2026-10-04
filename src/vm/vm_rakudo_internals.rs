//! VM-side dispatch for the `Rakudo::Internals` type-object methods that
//! ecosystem code reaches for.
//!
//! `Rakudo::Internals` is a core Rakudo class (`raku -e 'say
//! Rakudo::Internals.IS-WIN'` resolves with no `use`). Only the handful of
//! methods real distributions call are answered here; anything else falls
//! through to ordinary method resolution and its `X::Method::NotFound`.
//! (`Rakudo::Internals::JSON` is a separate class, see `vm_native_json`.)

use super::*;

/// When the process started, in nanoseconds since the epoch: recorded by the
/// first interpreter's construction (`Interpreter::new` calls
/// [`process_start_epoch_nanos`]), which is what Rakudo's `INITTIME` records.
static PROCESS_START_NANOS: std::sync::OnceLock<i64> = std::sync::OnceLock::new();

/// The process start time (see [`PROCESS_START_NANOS`]).
// Cost: O(1).
pub(crate) fn process_start_epoch_nanos() -> i64 {
    *PROCESS_START_NANOS.get_or_init(crate::builtins::epoch_nanos)
}

impl Interpreter {
    /// Dispatch a `Rakudo::Internals.<method>` call. Returns `None` when the
    /// invocant is not the `Rakudo::Internals` type object or the method is not
    /// one of the ones answered here.
    ///
    /// - `IS-WIN` / `IS-MACOS`: platform predicates (NativeLibs picks the
    ///   library name by `Rakudo::Internals.IS-WIN()`), from the host target.
    /// - `INITTIME`: when the process started, as a `Num` of seconds since
    ///   the epoch (Telemetry measures its wallclock column from it).
    /// - `INCLUDE`: the `-I` paths the running process was started with, as a
    ///   `List` of `Str`, read from `%*COMPILING<%?OPTIONS><I>` exactly as
    ///   Rakudo does. Test suites use it to re-exec `$*EXECUTABLE` with the
    ///   same include path (RakuDoc::Test::Files' `t/01-methods.rakutest`).
    pub(crate) fn try_rakudo_internals_method(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if !matches!(method, "IS-WIN" | "IS-MACOS" | "INCLUDE" | "INITTIME") {
            return None;
        }
        let ValueView::Package(name) = target.view() else {
            return None;
        };
        if name.resolve() != "Rakudo::Internals" {
            return None;
        }
        let (clean_args, _) = self.sanitize_call_args(args);
        if !clean_args.is_empty() {
            return None;
        }
        Some(Ok(match method {
            // Cost: O(1).
            "IS-WIN" => Value::truth(cfg!(target_os = "windows")),
            // Cost: O(1).
            "IS-MACOS" => Value::truth(cfg!(target_os = "macos")),
            // Cost: O(n), n = number of `-I` paths.
            "INCLUDE" => self.rakudo_internals_include(),
            // Cost: O(1).
            "INITTIME" => Value::num(process_start_epoch_nanos() as f64 / 1e9),
            _ => unreachable!(),
        }))
    }

    /// `%*COMPILING<%?OPTIONS><I>` as a `List`: absent is the empty list, a
    /// single `-I` is stored as a bare `Str`, several as an array.
    fn rakudo_internals_include(&self) -> Value {
        let include = self.env().get("%*COMPILING").and_then(|compiling| {
            let ValueView::Hash(compiling) = compiling.view() else {
                return None;
            };
            let ValueView::Hash(options) = compiling.map.get("%?OPTIONS")?.view() else {
                return None;
            };
            options.map.get("I").cloned()
        });
        let items = match include {
            None => Vec::new(),
            Some(value) => match value.view() {
                ValueView::Array(items, ..) => items.to_vec(),
                _ => vec![value],
            },
        };
        Value::array(items)
    }
}
