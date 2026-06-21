//! VM-side dispatch for the `JSON::Fast` / `JSON::Tiny` `to-json` / `from-json`
//! routines.
//!
//! These are not Raku core builtins — they are provided by the JSON modules.
//! The real `JSON::Fast` depends on ~50 `nqp::` ops mutsu lacks, so mutsu ships
//! native Rust implementations (`runtime/json.rs`) gated behind `use JSON::Fast`
//! / `use JSON::Tiny`, mirroring the native `Test` dispatch in `vm_native_test`.

use super::*;
use crate::runtime::json::{self, ToJsonOpts};
use crate::value::Value;

impl Interpreter {
    /// Dispatch `to-json` / `from-json` straight to the native JSON
    /// implementation when a JSON module is loaded. Returns `Some(result)` when
    /// handled, `None` to let the caller fall through unchanged (so a
    /// user-defined `sub to-json { … }` still wins, since user resolution runs
    /// before this).
    pub(super) fn try_native_json_function(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if !matches!(name, "to-json" | "from-json") || !self.json_module_loaded() {
            return None;
        }
        // Strip the synthetic `__test_callsite_line` trailer (and any other
        // call-site bookkeeping) the same way the Test path does.
        let (clean_args, _callsite_line) = self.sanitize_call_args(args);
        match name {
            "to-json" => Some(native_to_json(&clean_args)),
            "from-json" => Some(native_from_json(&clean_args)),
            _ => None,
        }
    }
}

fn native_to_json(args: &[Value]) -> Result<Value, RuntimeError> {
    let mut opts = ToJsonOpts::default();
    let mut subject: Option<&Value> = None;
    for arg in args {
        match arg {
            Value::Pair(name, val) => apply_to_json_named(&mut opts, name, val),
            Value::ValuePair(key, val) => {
                apply_to_json_named(&mut opts, &key.to_string_value(), val)
            }
            other if subject.is_none() => subject = Some(other),
            _ => {}
        }
    }
    let subject = subject.unwrap_or(&Value::Nil);
    Ok(Value::str(json::to_json(subject, &opts)))
}

fn apply_to_json_named(opts: &mut ToJsonOpts, name: &str, val: &Value) {
    match name {
        "pretty" => opts.pretty = val.truthy(),
        "sorted-keys" => opts.sorted_keys = val.truthy(),
        "spacing" => {
            if let Value::Int(n) = val {
                opts.spacing = (*n).max(0) as usize;
            }
        }
        _ => {}
    }
}

fn native_from_json(args: &[Value]) -> Result<Value, RuntimeError> {
    let text = args
        .iter()
        .find(|a| !matches!(a, Value::Pair(..) | Value::ValuePair(..)))
        .map(|v| v.to_string_value())
        .unwrap_or_default();
    json::from_json(&text).map_err(RuntimeError::new)
}
