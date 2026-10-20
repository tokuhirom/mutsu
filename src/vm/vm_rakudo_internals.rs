//! Support for the `Rakudo::Internals` type-object rows
//! (`builtins::method_table::instances::rakudo_internals`): the process start
//! time and the `-I` list. (`Rakudo::Internals::JSON` is a separate class, see
//! `vm_native_json`.)

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
    /// `%*COMPILING<%?OPTIONS><I>` as a `List`: absent is the empty list, a
    /// single `-I` is stored as a bare `Str`, several as an array.
    pub(crate) fn rakudo_internals_include(&self) -> Value {
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
