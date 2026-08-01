//! The process/introspection half of the `nqp::` value-op table.
//!
//! Split out of [`nqp_ops`](super::nqp_ops) (which stays focused on native
//! arithmetic, buffers and low-level file handles) to keep both files under the
//! 500-line limit. Reached from the same dispatch point: `call_nqp_op` falls
//! through to `call_nqp_op_process` before the loud unsupported-op error.
//!
//! The driver is rakudo's own `lib/Test.rakumod`, which mutsu still provides
//! natively (`runtime/test_functions.rs`). Running the genuine upstream module
//! instead needs exactly these ops — see
//! `todo/tickets/vendor-real-test-module.md`. Note that `can`, `join`, `split`
//! and `time` all collide with same-named Raku builtins of *different*
//! semantics, which is why they are implemented here under their full `nqp::`
//! name rather than by relaxing the aliasing guard in
//! `builtins_operators_fallback.rs`.

use crate::runtime::{Interpreter, IoHandleTarget, RuntimeError};
use crate::value::{Value, ValueView};

impl Interpreter {
    /// Try a process-level / introspection `nqp::` op. `None` means "not an op
    /// this table knows"; the caller then raises the unsupported-op error.
    pub(crate) fn call_nqp_op_process(
        &mut self,
        op: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        Some(match op {
            // -- the process's standard handles --
            // These are the *process* streams, deliberately not the `$*OUT` /
            // `$*ERR` dynamic variables: a caller that swapped `$*OUT` for a
            // capture object still gets the real stdout here, which is the
            // whole reason nqp code reaches for them (Test.rakumod unbuffers
            // the real streams so TAP output cannot be reordered).
            "getstdout" => Ok(self.std_handle(IoHandleTarget::Stdout)),
            "getstderr" => Ok(self.std_handle(IoHandleTarget::Stderr)),
            "getstdin" => Ok(self.std_handle(IoHandleTarget::Stdin)),

            // nqp::setbuffersizefh($fh, $size) — set the handle's output
            // buffer capacity (0 = unbuffered) and return the handle. Maps
            // onto the same state as Raku's `$fh.out-buffer = $size`, so any
            // pending bytes are flushed before the capacity changes.
            "setbuffersizefh" => {
                let fh = args.first().cloned().unwrap_or(Value::NIL);
                let size = args
                    .get(1)
                    .and_then(Self::parse_out_buffer_size)
                    .unwrap_or(0);
                match self.with_handle_mut(&fh, |state| state.out_buffer_setting(Some(Some(size))))
                {
                    Ok(_) => Ok(fh),
                    Err(e) => Err(e),
                }
            }

            // nqp::time — wall clock as an integer number of NANOseconds since
            // the epoch (MoarVM's `time`, which replaced the older float-valued
            // `time_n`).
            "time" => Ok(Value::int(Self::epoch_nanos())),

            // nqp::eqaddr($a, $b) — object identity as an int 0/1. Same
            // relation as Raku's `=:=`, which is already identity over the
            // container-kind values and by-name over type objects.
            "eqaddr" => Ok(Value::int(i64::from(crate::runtime::values_identical(
                args.first().unwrap_or(&Value::NIL),
                args.get(1).unwrap_or(&Value::NIL),
            )))),

            // nqp::can($obj, $name) — int 0/1: does this object have a method
            // of that name (the low-level form of `$obj.^can($name)`).
            "can" => {
                let target = args.first().cloned().unwrap_or(Value::NIL);
                let name = args.get(1).map(|v| v.to_string_value()).unwrap_or_default();
                let found = !self.collect_can_methods(&target, &name).is_empty();
                Ok(Value::int(i64::from(found)))
            }

            // nqp::join($sep, $list) / nqp::split($sep, $str) — string join and
            // split over an nqp list. Plain literal separators, no regex and no
            // Raku `split` adverbs: `nqp::split("", $s)` yields the characters,
            // splitting the empty string yields the empty list, and every
            // separator occurrence produces a field (so trailing empties are
            // kept).
            "join" => {
                let sep = args
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let parts = args.get(1).map(Self::nqp_list_strings).unwrap_or_default();
                Ok(Value::str(parts.join(&sep)))
            }
            "split" => {
                let sep = args
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let target = args.get(1).map(|v| v.to_string_value()).unwrap_or_default();
                let parts: Vec<Value> = if target.is_empty() {
                    Vec::new()
                } else if sep.is_empty() {
                    target.chars().map(|c| Value::str(c.to_string())).collect()
                } else {
                    target
                        .split(&sep)
                        .map(|s| Value::str(s.to_string()))
                        .collect()
                };
                Ok(Value::array(parts))
            }

            _ => return None,
        })
    }

    /// Wall clock in nanoseconds since the Unix epoch, saturating rather than
    /// wrapping (an `i64` of nanoseconds runs out in the year 2262).
    fn epoch_nanos() -> i64 {
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| i64::try_from(d.as_nanos()).unwrap_or(i64::MAX))
            .unwrap_or(0)
    }

    /// The process-level handle for one of the standard streams: the handle
    /// registered by `init_io_environment`, found by target rather than by the
    /// `$*OUT`/`$*ERR`/`$*IN` dynamic variables (which a caller may have
    /// rebound). Lowest id wins, so it is the one created at startup.
    fn std_handle(&mut self, target: IoHandleTarget) -> Value {
        let existing = {
            let table = self.io_handles();
            table
                .map
                .iter()
                .filter(|(_, state)| state.target == target)
                .map(|(id, _)| *id)
                .min()
        };
        match existing {
            Some(id) => self.make_handle_instance(id),
            // No startup handle (a bare embedding of the interpreter): make one
            // rather than handing back a Nil the caller cannot use.
            None => {
                let mode = match target {
                    IoHandleTarget::Stdin => crate::runtime::IoHandleMode::Read,
                    _ => crate::runtime::IoHandleMode::Write,
                };
                self.create_handle(target, mode, None)
            }
        }
    }

    /// The elements of an nqp list as strings. nqp's `join` takes a VM list;
    /// mutsu represents one as an ordinary `Array`/`List` value, and a single
    /// non-list value counts as a one-element list.
    fn nqp_list_strings(value: &Value) -> Vec<String> {
        match value.view() {
            ValueView::Array(items, _) => items.iter().map(|v| v.to_string_value()).collect(),
            ValueView::Slip(items) => items.iter().map(|v| v.to_string_value()).collect(),
            _ => vec![value.to_string_value()],
        }
    }
}
