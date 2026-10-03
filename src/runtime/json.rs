//! The JSON codec behind `Rakudo::Internals::JSON.to-json` / `.from-json`.
//!
//! That class is **core Rakudo**, not an ecosystem module — it resolves with no
//! `use` — so this is ordinary core surface, outside ADR-0096 §D4's rung-3
//! ledger. The name-keyed `JSON::Fast` / `JSON::Tiny` providers this file used
//! to serve are gone: both are vendored batteries now and run their own
//! upstream source (#8183/#8203 and #8226). See `vm/vm_native_json.rs` for the
//! dispatch side.
//!
//! Encoding follows JSON::Fast 0.19 semantics: `:pretty` defaults to True with a
//! 2-space indent, type objects / undefined values render as `null`, `Rat`s gain
//! a trailing `.0` when integral, and `Num`s gain a trailing `e0` when they carry
//! no exponent. Decoding maps `true`/`false` to `Bool`, `null` to the `Any` type
//! object, integers to `Int`, decimals to `Rat`, and exponential forms to `Num`.

use crate::value::{Value, ValueView};

/// Bound JSON recursion before it can exhaust a Rust thread's stack.
pub(crate) const MAX_JSON_DEPTH: usize = 256;

/// Options controlling `to-json` rendering. Mirrors the JSON::Fast named params.
pub(crate) struct ToJsonOpts {
    pub pretty: bool,
    pub sorted_keys: bool,
    pub spacing: usize,
    /// `:enums-as-value` — serialize enum values as their underlying payload
    /// (`0` / `"Eins"`) instead of their short name.
    pub enums_as_value: bool,
    /// `$*JSON_NAN_INF_SUPPORT` — emit `NaN`/`Inf`/`-Inf` instead of `null`.
    pub nan_inf_support: bool,
}

impl Default for ToJsonOpts {
    fn default() -> Self {
        ToJsonOpts {
            pretty: true,
            sorted_keys: false,
            spacing: 2,
            enums_as_value: false,
            nan_inf_support: false,
        }
    }
}

/// Serialize a `Value` to a JSON string.
// Cost: O(n log n + b), n = values encoded, b = output bytes.
pub(crate) fn to_json(val: &Value, opts: &ToJsonOpts) -> Result<String, String> {
    let mut out = String::new();
    jsonify(val, opts, 0, 0, &mut out)?;
    Ok(out)
}

fn indent(out: &mut String, opts: &ToJsonOpts, level: usize) {
    if opts.pretty {
        for _ in 0..(level * opts.spacing) {
            out.push(' ');
        }
    }
}

/// Is `class_name` the `Rational` role's pun — either the bare role name or one
/// of its parameterisations (`Rational[Int,Int]`)?
fn rational_pun_class(class_name: &str) -> bool {
    class_name
        .split_once('[')
        .map(|(base, _)| base)
        .unwrap_or(class_name)
        == "Rational"
}

/// The `Rat` a Rational pun's numerator/denominator pair denotes.
fn rational_as_rat(numerator: &Value, denominator: &Value) -> Value {
    crate::value::make_rat(
        numerator.as_int().unwrap_or(0),
        denominator.as_int().unwrap_or(1).max(1),
    )
}

fn jsonify(
    val: &Value,
    opts: &ToJsonOpts,
    level: usize,
    depth: usize,
    out: &mut String,
) -> Result<(), String> {
    if depth >= MAX_JSON_DEPTH {
        return Err(format!("JSON nesting exceeds {MAX_JSON_DEPTH} levels"));
    }
    match val.view() {
        ValueView::Bool(b) => out.push_str(if b { "true" } else { "false" }),
        ValueView::Int(_) | ValueView::BigInt(_) => out.push_str(&val.to_string_value()),
        ValueView::Rat(..) | ValueView::FatRat(..) | ValueView::BigRat(..) => {
            // JSON::Fast: emit the Rat string, appending ".0" when it has no
            // decimal point so the value reads back as a Rat, not an Int.
            let s = val.to_string_value();
            out.push_str(&s);
            if !s.contains('.') {
                out.push_str(".0");
            }
        }
        ValueView::Num(f) => {
            if f.is_nan() || f.is_infinite() {
                // JSON has no NaN/Inf; JSON::Fast emits `null` unless the dynamic
                // var $*JSON_NAN_INF_SUPPORT is set.
                if opts.nan_inf_support {
                    out.push_str(if f.is_nan() {
                        "NaN"
                    } else if f > 0.0 {
                        "Inf"
                    } else {
                        "-Inf"
                    });
                } else {
                    out.push_str("null");
                }
            } else {
                let s = val.to_string_value();
                out.push_str(&s);
                if !s.contains('e') && !s.contains('E') {
                    out.push_str("e0");
                }
            }
        }
        ValueView::Str(s) => {
            out.push('"');
            escape_str(&s, out);
            out.push('"');
        }
        ValueView::Scalar(inner) => jsonify(inner, opts, level, depth + 1, out)?,
        ValueView::Mixin(inner, mixins) => {
            // A punned Rational-role instance is Real; JSON::Fast's Real:D
            // candidate serializes it numerically (0.3), not as an opaque
            // string. A bare-role pun keeps its attrs in the mixin map.
            if mixins.contains_key("__mutsu_role__Rational")
                && let (Some(n), Some(d)) = (
                    mixins.role_attribute("Rational", "numerator"),
                    mixins.role_attribute("Rational", "denominator"),
                )
            {
                jsonify(&rational_as_rat(&n, &d), opts, level, depth + 1, out)?;
            } else {
                jsonify(inner, opts, level, depth + 1, out)?;
            }
        }
        ValueView::Array(arr, _) => jsonify_seq(arr.items(), opts, level, depth, out)?,
        ValueView::Seq(items) | ValueView::HyperSeq(items) | ValueView::RaceSeq(items) => {
            jsonify_seq(&items, opts, level, depth, out)?;
        }
        ValueView::Slip(items) => jsonify_seq(&items, opts, level, depth, out)?,
        ValueView::Hash(h) => {
            let entries: Vec<(&String, &Value)> = h.map.iter().collect();
            jsonify_object(entries, opts, level, depth, out)?;
        }
        ValueView::Pair(k, v) => {
            jsonify_object(vec![(k, v)], opts, level, depth, out)?;
        }
        ValueView::ValuePair(k, v) => {
            let key = k.to_string_value();
            jsonify_object(vec![(&key, v)], opts, level, depth, out)?;
        }
        // Enum values: short name by default, underlying payload with
        // `:enums-as-value` (JSON::Fast semantics).
        ValueView::Enum { key, value, .. } => {
            if opts.enums_as_value {
                jsonify(&value.to_value(), opts, level, depth + 1, out)?;
            } else {
                out.push('"');
                escape_str(&key.resolve(), out);
                out.push('"');
            }
        }
        // Instant serializes as its `.DateTime` ISO string (JSON::Fast's
        // Instant candidate: `"{.DateTime}"`), not the raw `Instant:...` gist.
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if class_name.resolve() == "Instant" => {
            use crate::builtins::methods_0arg::temporal;
            let tai = attributes
                .as_map()
                .get("value")
                .map(|v| v.to_f64())
                .unwrap_or(0.0);
            let posix = temporal::instant_to_posix(tai);
            let total_i = posix.floor() as i64;
            let frac = posix - total_i as f64;
            let day_secs = total_i.rem_euclid(86400);
            let epoch_days = (total_i - day_secs) / 86400;
            let (y, m, d) = temporal::epoch_days_to_civil(epoch_days);
            let iso = temporal::format_datetime(
                y,
                m,
                d,
                day_secs / 3600,
                (day_secs % 3600) / 60,
                (day_secs % 60) as f64 + frac,
                0,
            );
            out.push('"');
            escape_str(&iso, out);
            out.push('"');
        }
        // The *parameterised* Rational pun (`Rational[Int,Int].new(3, 10)`) is a
        // real instance of the punned class, so its numerator/denominator are
        // ordinary attributes rather than the mixin markers handled above.
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if rational_pun_class(&class_name.resolve()) => {
            let attrs = attributes.as_map();
            match (attrs.get("numerator"), attrs.get("denominator")) {
                (Some(n), Some(d)) => {
                    jsonify(&rational_as_rat(n, d), opts, level, depth + 1, out)?;
                }
                _ => out.push_str("null"),
            }
        }
        // Duration is Real (a Rat-backed instance: `value` attr); JSON::Fast's
        // Real:D candidate serializes it numerically as a Num (`57e0`).
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if class_name.resolve() == "Duration" => {
            let num = attributes
                .as_map()
                .get("value")
                .map(|v| v.to_f64())
                .unwrap_or(0.0);
            jsonify(&Value::num(num), opts, level, depth + 1, out)?;
        }
        // Type objects / undefined values render as JSON null.
        ValueView::Nil | ValueView::Package(_) | ValueView::Whatever | ValueView::HyperWhatever => {
            out.push_str("null");
        }
        // Anything else: fall back to a quoted stringification. Raku would die on
        // an unjsonifiable object; being lenient keeps web/template use working.
        _ => {
            out.push('"');
            escape_str(&val.to_string_value(), out);
            out.push('"');
        }
    }
    Ok(())
}

fn jsonify_seq(
    items: &[Value],
    opts: &ToJsonOpts,
    level: usize,
    depth: usize,
    out: &mut String,
) -> Result<(), String> {
    if items.is_empty() {
        // JSON::Fast prints an empty array as "[\n]" in pretty mode.
        if opts.pretty {
            out.push_str("[\n");
            indent(out, opts, level);
            out.push(']');
        } else {
            out.push_str("[]");
        }
        return Ok(());
    }
    out.push('[');
    if opts.pretty {
        out.push('\n');
    }
    for (i, item) in items.iter().enumerate() {
        if i > 0 {
            out.push(',');
            if opts.pretty {
                out.push('\n');
            }
        }
        indent(out, opts, level + 1);
        jsonify(item, opts, level + 1, depth + 1, out)?;
    }
    if opts.pretty {
        out.push('\n');
        indent(out, opts, level);
    }
    out.push(']');
    Ok(())
}

fn jsonify_object(
    entries: Vec<(&String, &Value)>,
    opts: &ToJsonOpts,
    level: usize,
    depth: usize,
    out: &mut String,
) -> Result<(), String> {
    if entries.is_empty() {
        if opts.pretty {
            out.push_str("{\n");
            indent(out, opts, level);
            out.push('}');
        } else {
            out.push_str("{}");
        }
        return Ok(());
    }
    let mut entries = entries;
    if opts.sorted_keys {
        entries.sort_by(|a, b| a.0.cmp(b.0));
    }
    out.push('{');
    if opts.pretty {
        out.push('\n');
    }
    for (i, (k, v)) in entries.iter().enumerate() {
        if i > 0 {
            out.push(',');
            if opts.pretty {
                out.push('\n');
            }
        }
        indent(out, opts, level + 1);
        out.push('"');
        escape_str(k, out);
        out.push_str("\":");
        if opts.pretty {
            out.push(' ');
        }
        jsonify(v, opts, level + 1, depth + 1, out)?;
    }
    if opts.pretty {
        out.push('\n');
        indent(out, opts, level);
    }
    out.push('}');
    Ok(())
}

/// Escape a string per JSON::Fast's `str-escape`: `\n`/`\r`/`\t` and `"`/`\\`
/// get short escapes, other control chars (< 0x20) become `\u00xx`, code points
/// above the BMP become surrogate pairs, everything else passes through literally.
fn escape_str(s: &str, out: &mut String) {
    for c in s.chars() {
        match c {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            c if (c as u32) < 0x20 => {
                out.push_str(&format!("\\u{:04x}", c as u32));
            }
            c if (c as u32) >= 0x10000 => {
                // JSON::Fast's to-surrogate-pair uses .base(16): UPPERCASE hex
                // (control-char escapes above stay lowercase, fmt "\u%04x").
                let cp = c as u32 - 0x10000;
                let hi = 0xD800 + (cp >> 10);
                let lo = 0xDC00 + (cp & 0x3FF);
                out.push_str(&format!("\\u{:04X}\\u{:04X}", hi, lo));
            }
            c => out.push(c),
        }
    }
}

mod decode;
pub(crate) use decode::{FromJsonError, from_json};
