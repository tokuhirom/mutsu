//! VM-side dispatch for `Rakudo::Internals::JSON.to-json` / `.from-json`.
//!
//! This is a **core Rakudo class**, not an ecosystem module: `raku -e 'say
//! Rakudo::Internals::JSON.to-json({a => 1})'` resolves with no `use`, so
//! answering it from Rust is ordinary core surface, not a BATTERIES.md rung-3
//! provider. zef reaches for it on every metadata read
//! (`vendor/zef/lib/Zef.rakumod`), and so do OpenSSL's `%?RESOURCES` loading
//! and JSON::JWT.
//!
//! The `JSON::Fast` / `JSON::Tiny` name-keyed providers that used to live here
//! are **gone**. Both are vendored batteries now and run their own upstream
//! source like any other module: `JSON::Tiny` since #8183/#8203,
//! `JSON::Fast` since #8226 (`modules/JSON-Fast/`). (`Test` went the same way
//! in #7566.)

use super::*;
use crate::runtime::json::{self, ToJsonOpts};
use crate::value::Value;

impl Interpreter {
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
                let items = arr.items().clone();
                items.iter().any(|i| self.json_subject_needs_prepare(i))
            }
            ValueView::Seq(items) => {
                let items = items.to_vec();
                items.iter().any(|i| self.json_subject_needs_prepare(i))
            }
            ValueView::Slip(items) => {
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
                    ValueView::Array(arr, _) => arr.items().clone(),
                    ValueView::Seq(items) => items.to_vec(),
                    ValueView::Slip(items) => items.to_vec(),
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
                    .items()
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

    /// Base `to-json` options for this call site. The `use JSON::Fast
    /// <immutable !pretty ...>` import list used to seed these; that provider
    /// is gone (#8226), and `Rakudo::Internals::JSON` has no import list of its
    /// own, so only the `$*JSON_NAN_INF_SUPPORT` dynamic variable remains.
    /// Explicit named args override these in `native_to_json`.
    fn base_to_json_opts(&self) -> ToJsonOpts {
        ToJsonOpts {
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
            "to-json" => {
                let clean_args = self.prepare_to_json_args(clean_args);
                native_to_json(&clean_args, self.base_to_json_opts())
            }
            // `Rakudo::Internals::JSON.from-json(Str)` takes no `:immutable`.
            "from-json" => native_from_json(&clean_args, false),
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
        // JSON::Fast's own `from-json` just `die`s a string on a parse
        // failure, so a plain X::AdHoc is the faithful shape. (The real
        // `JSON::Tiny` throws `X::JSON::Tiny::Invalid` instead — its own
        // vendored source now does that itself, and this provider no longer
        // has to guess which module's error shape the caller wanted.)
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
