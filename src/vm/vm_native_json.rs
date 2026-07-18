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
    pub(crate) fn try_native_json_function(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if !matches!(name, "to-json" | "from-json") || !self.json_module_loaded() {
            return None;
        }
        // Strip the synthetic `__test_callsite_line` trailer (and any other
        // call-site bookkeeping) the same way the Test path does. Unwrap
        // VarRef-wrapped args (a statement-position call compiles its variable
        // args through `compile_call_arg`, which wraps them for rw detection)
        // — the serializer would otherwise hit its opaque-value fallback and
        // emit the *stringification* of the wrapped Hash.
        let (clean_args, _callsite_line) = self.sanitize_call_args(args);
        let clean_args: Vec<Value> = clean_args
            .into_iter()
            .map(crate::runtime::types::unwrap_varref_value)
            .collect();
        match name {
            "to-json" => {
                let clean_args = self.prepare_to_json_args(clean_args);
                Some(native_to_json(&clean_args, self.base_to_json_opts()))
            }
            "from-json" => Some(native_from_json(
                &clean_args,
                self.json_import_defaults.immutable,
            )),
            _ => None,
        }
    }

    /// Pre-convert to-json subject args: user instances doing Associative
    /// (or Positional) serialize via their `.list` (JSON::Fast's
    /// pretty/unpretty-associative and -positional iterate exactly that), so
    /// deep-replace them with plain Hash/Array values the pure serializer
    /// understands. Named (Pair) args are left alone.
    fn prepare_to_json_args(&mut self, args: Vec<Value>) -> Vec<Value> {
        args.into_iter()
            .map(|a| match a.view() {
                ValueView::Pair(..) | ValueView::ValuePair(..) => a,
                _ if self.json_subject_needs_prepare(&a) => self.prepare_to_json_subject(&a),
                _ => a,
            })
            .collect()
    }

    /// Cheap read-only scan: does the subject contain an Associative/Positional
    /// user instance anywhere? Avoids deep-rebuilding plain data.
    fn json_subject_needs_prepare(&mut self, val: &Value) -> bool {
        match val.view() {
            ValueView::Instance { .. }
            | ValueView::Mixin(..)
            | ValueView::CustomTypeInstance(..) => {
                self.type_matches_value("Associative", val)
                    || self.type_matches_value("Positional", val)
            }
            ValueView::Array(arr, _) => {
                let items = arr.items.clone();
                items.iter().any(|i| self.json_subject_needs_prepare(i))
            }
            ValueView::Seq(items) | ValueView::Slip(items) => {
                let items = items.to_vec();
                items.iter().any(|i| self.json_subject_needs_prepare(i))
            }
            ValueView::Hash(h) => {
                let values: Vec<Value> = h.map.values().cloned().collect();
                values.iter().any(|v| self.json_subject_needs_prepare(v))
            }
            ValueView::Scalar(inner) => {
                let inner = inner.clone();
                self.json_subject_needs_prepare(&inner)
            }
            _ => false,
        }
    }

    fn prepare_to_json_subject(&mut self, val: &Value) -> Value {
        match val.view() {
            ValueView::Instance { .. }
            | ValueView::Mixin(..)
            | ValueView::CustomTypeInstance(..) => {
                let assoc = self.type_matches_value("Associative", val);
                let positional = self.type_matches_value("Positional", val);
                if !assoc && !positional {
                    return val.clone();
                }
                let Ok(listed) = self.call_method_with_values(val.clone(), "list", vec![]) else {
                    return val.clone();
                };
                let items: Vec<Value> = match listed.view() {
                    ValueView::Array(arr, _) => arr.items.clone(),
                    ValueView::Seq(items) | ValueView::Slip(items) => items.to_vec(),
                    _ => return val.clone(),
                };
                if assoc {
                    // Associative wins over Positional (JSON::Fast dispatch
                    // order); elements are Pairs.
                    let mut map = std::collections::HashMap::new();
                    for item in &items {
                        match item.view() {
                            ValueView::Pair(k, v) => {
                                map.insert(k.clone(), self.prepare_to_json_subject(v));
                            }
                            ValueView::ValuePair(k, v) => {
                                map.insert(k.to_string_value(), self.prepare_to_json_subject(v));
                            }
                            _ => {}
                        }
                    }
                    Value::hash_with_data(Value::hash_arc(map))
                } else {
                    let items = items
                        .iter()
                        .map(|i| self.prepare_to_json_subject(i))
                        .collect();
                    Value::real_array(items)
                }
            }
            ValueView::Array(arr, kind) => {
                let items: Vec<Value> = arr
                    .items
                    .iter()
                    .map(|i| self.prepare_to_json_subject(i))
                    .collect();
                Value::array_with_kind(
                    crate::gc::Gc::new(crate::value::ArrayData::new(items)),
                    kind,
                )
            }
            ValueView::Hash(h) => {
                let map: std::collections::HashMap<String, Value> = h
                    .map
                    .iter()
                    .map(|(k, v)| (k.clone(), self.prepare_to_json_subject(v)))
                    .collect();
                Value::hash_with_data(Value::hash_arc(map))
            }
            ValueView::Scalar(inner) => {
                let inner = inner.clone();
                self.prepare_to_json_subject(&inner)
            }
            _ => val.clone(),
        }
    }

    /// Base `to-json` options for this call site: the import-list defaults of
    /// the latest `use JSON::Fast <...>`, plus the `$*JSON_NAN_INF_SUPPORT`
    /// dynamic variable. Explicit named args override these in
    /// `native_to_json`.
    fn base_to_json_opts(&self) -> ToJsonOpts {
        let d = self.json_import_defaults;
        ToJsonOpts {
            pretty: !d.not_pretty,
            sorted_keys: d.sorted_keys,
            enums_as_value: d.enums_as_value,
            nan_inf_support: self
                .get_dynamic_var("*JSON_NAN_INF_SUPPORT")
                .map(|v| v.truthy())
                .unwrap_or(false),
            ..ToJsonOpts::default()
        }
    }

    /// Dispatch `Rakudo::Internals::JSON.from-json` / `.to-json`. Unlike the
    /// module-provided `to-json`/`from-json` subs, this is a core Rakudo class
    /// always available (used by e.g. OpenSSL's `%?RESOURCES` loading and
    /// JSON::JWT), so it is not gated on a JSON module being loaded. Returns
    /// `Some` when the invocant is the `Rakudo::Internals::JSON` type object and
    /// the method is one of the two JSON routines.
    pub(crate) fn try_rakudo_internals_json_method(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if !matches!(method, "to-json" | "from-json") {
            return None;
        }
        let ValueView::Package(name) = target.view() else {
            return None;
        };
        if name.resolve() != "Rakudo::Internals::JSON" {
            return None;
        }
        let (clean_args, _) = self.sanitize_call_args(args);
        Some(match method {
            "to-json" => native_to_json(&clean_args, self.base_to_json_opts()),
            "from-json" => native_from_json(&clean_args, self.json_import_defaults.immutable),
            _ => unreachable!(),
        })
    }
}

fn native_to_json(args: &[Value], base_opts: ToJsonOpts) -> Result<Value, RuntimeError> {
    let mut opts = base_opts;
    let mut subject: Option<&Value> = None;
    for arg in args {
        match arg.view() {
            ValueView::Pair(name, val) => apply_to_json_named(&mut opts, name, val),
            ValueView::ValuePair(key, val) => {
                apply_to_json_named(&mut opts, &key.to_string_value(), val)
            }
            _ if subject.is_none() => subject = Some(arg),
            _ => {}
        }
    }
    let subject = subject.unwrap_or(&crate::value::NIL_VALUE);
    Ok(Value::str(json::to_json(subject, &opts)))
}

fn apply_to_json_named(opts: &mut ToJsonOpts, name: &str, val: &Value) {
    match name {
        "pretty" => opts.pretty = val.truthy(),
        "sorted-keys" => opts.sorted_keys = val.truthy(),
        "enums-as-value" => opts.enums_as_value = val.truthy(),
        "spacing" => {
            if let ValueView::Int(n) = val.view() {
                opts.spacing = n.max(0) as usize;
            }
        }
        _ => {}
    }
}

fn native_from_json(args: &[Value], default_immutable: bool) -> Result<Value, RuntimeError> {
    let mut immutable = default_immutable;
    let mut allow_jsonc = false;
    let mut text = String::new();
    let mut have_text = false;
    let mut named = |name: &str, val: &Value| match name {
        "immutable" => immutable = val.truthy(),
        "allow-jsonc" => allow_jsonc = val.truthy(),
        _ => {}
    };
    for arg in args {
        match arg.view() {
            ValueView::Pair(name, val) => named(name, val),
            ValueView::ValuePair(key, val) => named(&key.to_string_value(), val),
            _ if !have_text => {
                text = arg.to_string_value();
                have_text = true;
            }
            _ => {}
        }
    }
    json::from_json(&text, immutable, allow_jsonc).map_err(|e| match e {
        json::FromJsonError::Parse(msg) => RuntimeError::new(msg),
        json::FromJsonError::AdditionalContent {
            parsed,
            parsed_length,
            rest_position,
        } => {
            // Mirror JSON::Fast's X::JSON::AdditionalContent so multi-document
            // consumers can catch it and resume from `.rest-position`.
            let msg = format!(
                "JSON Input contained additional text after the document \
                 (parsed {parsed_length} chars, next non-whitespace lives at {rest_position})"
            );
            let ex = Value::make_exception(
                "X::JSON::AdditionalContent",
                &[
                    ("parsed", parsed),
                    ("parsed-length", Value::int(parsed_length as i64)),
                    ("rest-position", Value::int(rest_position as i64)),
                    ("message", Value::str(msg.clone())),
                ],
            );
            let mut err = RuntimeError::new(msg);
            err.exception = Some(Box::new(ex));
            err
        }
    })
}
