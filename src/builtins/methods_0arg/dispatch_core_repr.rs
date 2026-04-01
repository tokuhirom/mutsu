/// Representation methods: gist, raku, perl
use crate::runtime;
use crate::value::{RuntimeError, Value};

use super::raku_repr::raku_value;
use super::{format_temporal_num, gist_array_wrap, range_gist_string};

pub(super) fn dispatch(
    target: &Value,
    method: &str,
) -> Option<Option<Result<Value, RuntimeError>>> {
    if !matches!(method, "gist" | "raku" | "perl") {
        return None;
    }
    Some(match target {
        Value::Bool(b) => {
            if method == "gist" {
                Some(Ok(Value::str(
                    if *b { "True" } else { "False" }.to_string(),
                )))
            } else {
                Some(Ok(Value::str(
                    if *b { "Bool::True" } else { "Bool::False" }.to_string(),
                )))
            }
        }
        Value::Nil => {
            if method == "raku" || method == "perl" {
                Some(Ok(Value::str_from("Nil")))
            } else {
                // gist returns "(Any)" because mutsu uses Value::Nil
                // for uninitialized variables (which are actually Any).
                // TODO: fix variable initialization to use type objects
                // instead of Nil, then change gist to return "Nil".
                Some(Ok(Value::str_from("(Any)")))
            }
        }
        Value::FatRat(n, d) => {
            if *d == 0 && (method == "gist" || method == "Str") {
                Some(Err(RuntimeError::numeric_divide_by_zero_with(Some(
                    Value::Int(*n),
                ))))
            } else if method == "gist" {
                Some(Ok(Value::str(target.to_string_value())))
            } else {
                Some(Ok(Value::str(format!("FatRat.new({}, {})", n, d))))
            }
        }
        Value::BigRat(n, d) => {
            use num_traits::Zero;
            if d.is_zero() && (method == "gist" || method == "Str") {
                Some(Err(RuntimeError::numeric_divide_by_zero_with(Some(
                    Value::from_bigint(n.clone()),
                ))))
            } else if method == "gist" {
                Some(Ok(Value::str(target.to_string_value())))
            } else {
                Some(Ok(Value::str(raku_value(target))))
            }
        }
        Value::Rat(n, d) => {
            if *d == 0 {
                if method == "raku" || method == "perl" {
                    Some(Ok(Value::str(format!("<{}/0>", n))))
                } else {
                    Some(Err(RuntimeError::numeric_divide_by_zero_with(Some(
                        Value::Int(*n),
                    ))))
                }
            } else if *n % *d == 0 {
                if method == "raku" || method == "perl" {
                    // .raku on Rat always shows decimal: 27.0, not 27
                    Some(Ok(Value::str(format!("{}.0", *n / *d))))
                } else {
                    Some(Ok(Value::str(format!("{}", *n / *d))))
                }
            } else {
                if method == "gist" {
                    return Some(Some(Ok(Value::str(target.to_string_value()))));
                }
                let mut dd = *d;
                while dd % 2 == 0 {
                    dd /= 2;
                }
                while dd % 5 == 0 {
                    dd /= 5;
                }
                if dd == 1 {
                    let val = *n as f64 / *d as f64;
                    Some(Ok(Value::str(format!("{}", val))))
                } else {
                    Some(Ok(Value::str(format!("<{}/{}>", n, d))))
                }
            }
        }
        Value::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "Signature" => {
            let attr_key = if method == "gist" { "gist" } else { "raku" };
            Some(Ok(attributes
                .get(attr_key)
                .cloned()
                .unwrap_or_else(|| Value::str(format!("{}()", class_name)))))
        }
        Value::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "Parameter" && (method == "raku" || method == "perl") => Some(Ok(
            Value::str(crate::value::signature::parameter_to_raku(attributes)),
        )),
        Value::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "Failure" => {
            let msg = attributes
                .get("exception")
                .map(|v| v.to_string_value())
                .unwrap_or_else(|| "Failed".to_string());
            if method == "gist" {
                Some(Ok(Value::str(msg)))
            } else if method == "raku" || method == "perl" {
                let raku_str = if target.is_failure_handled() {
                    // For handled Failures, produce an expression that when
                    // EVALed creates a handled Failure, preserving the flag.
                    format!("do {{ my $f = Failure.new(\"{}\"); $f.Bool; $f }}", msg)
                } else {
                    format!("Failure.new(\"{}\")", msg)
                };
                Some(Ok(Value::str(raku_str)))
            } else {
                // Str, Numeric, Int, etc. -- using a Failure in a value
                // context throws the wrapped exception.
                if let Some(ex) = attributes.get("exception") {
                    Some(Err(RuntimeError::from_exception_value(ex.clone())))
                } else {
                    Some(Err(RuntimeError::new(&msg)))
                }
            }
        }
        Value::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "CallFrame" => {
            if method == "gist" {
                let file = attributes
                    .get("file")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let line = attributes
                    .get("line")
                    .map(|v| v.to_string_value())
                    .unwrap_or_else(|| "0".to_string());
                Some(Ok(Value::str(format!("{} at line {}", file, line))))
            } else {
                // .raku: CallFrame.new(...)
                let file = attributes
                    .get("file")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let line = attributes
                    .get("line")
                    .map(|v| v.to_string_value())
                    .unwrap_or_else(|| "0".to_string());
                Some(Ok(Value::str(format!(
                    "CallFrame.new(annotations => {{:file(\"{}\"), :line(\"{}\")}}, my => {{}})",
                    file, line
                ))))
            }
        }
        Value::Instance {
            class_name,
            attributes,
            ..
        } if crate::runtime::utils::is_buf_or_blob_class(&class_name.resolve()) => {
            if let Some(Value::Array(bytes, ..)) = attributes.get("bytes") {
                if method == "raku" || method == "perl" {
                    let elems: Vec<String> = bytes
                        .iter()
                        .map(|b| match b {
                            Value::Int(i) => i.to_string(),
                            _ => "0".to_string(),
                        })
                        .collect();
                    // Normalize short names to canonical forms for .raku
                    let cn = class_name.resolve();
                    let canonical = match cn.as_str() {
                        "buf8" => "Buf[uint8]",
                        "buf16" => "Buf[uint16]",
                        "buf32" => "Buf[uint32]",
                        "buf64" => "Buf[uint64]",
                        "blob8" => "Blob[uint8]",
                        "blob16" => "Blob[uint16]",
                        "blob32" => "Blob[uint32]",
                        "blob64" => "Blob[uint64]",
                        other => other,
                    };
                    Some(Ok(Value::str(format!(
                        "{}.new({})",
                        canonical,
                        elems.join(",")
                    ))))
                } else {
                    // gist
                    if bytes.is_empty() {
                        Some(Ok(Value::str(format!("{}()", class_name))))
                    } else {
                        let hex: Vec<String> = bytes
                            .iter()
                            .map(|b| match b {
                                Value::Int(i) => format!("{:02X}", *i as u8),
                                _ => "00".to_string(),
                            })
                            .collect();
                        Some(Ok(Value::str(format!(
                            "{}:0x<{}>",
                            class_name,
                            hex.join(" ")
                        ))))
                    }
                }
            } else {
                Some(Ok(Value::str(format!("{}()", class_name))))
            }
        }
        Value::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "Instant" && (method == "raku" || method == "perl") => {
            let tai = attributes.get("value").map(|v| v.to_f64()).unwrap_or(0.0);
            let posix = crate::builtins::methods_0arg::temporal::instant_to_posix(tai);
            Some(Ok(Value::str(format!(
                "Instant.from-posix({})",
                format_temporal_num(posix)
            ))))
        }
        Value::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "Duration" && (method == "raku" || method == "perl") => {
            let val = attributes.get("value").cloned().unwrap_or(Value::Num(0.0));
            Some(Ok(Value::str(format!(
                "Duration.new({})",
                format_temporal_num(val.to_f64())
            ))))
        }
        Value::Bag(b, _) => {
            let mut keys: Vec<(&String, &i64)> = b.iter().collect();
            keys.sort_by_key(|(k, _)| (*k).clone());
            let inner = keys
                .iter()
                .map(|(k, v)| {
                    if **v == 1 {
                        (*k).clone()
                    } else {
                        format!("{}({})", k, v)
                    }
                })
                .collect::<Vec<_>>()
                .join(" ");
            if method == "raku" || method == "perl" {
                let pairs = keys
                    .iter()
                    .map(|(k, v)| {
                        format!(
                            "\"{}\"=>{}",
                            k.replace('\\', "\\\\").replace('"', "\\\""),
                            v
                        )
                    })
                    .collect::<Vec<_>>()
                    .join(",");
                Some(Ok(Value::str(format!("({}).Bag", pairs))))
            } else {
                // gist: Bag(key(count) ...)
                Some(Ok(Value::str(format!("Bag({})", inner))))
            }
        }
        Value::Set(s, _) => {
            let mut keys: Vec<&String> = s.iter().collect();
            keys.sort();
            if method == "raku" || method == "perl" {
                let pairs = keys
                    .iter()
                    .map(|k| {
                        format!(
                            "\"{}\"=>Bool::True",
                            k.replace('\\', "\\\\").replace('"', "\\\"")
                        )
                    })
                    .collect::<Vec<_>>()
                    .join(",");
                Some(Ok(Value::str(format!("({}).Set", pairs))))
            } else {
                // gist: Set(a b c)
                let inner = keys
                    .iter()
                    .map(|k| k.as_str())
                    .collect::<Vec<_>>()
                    .join(" ");
                Some(Ok(Value::str(format!("Set({})", inner))))
            }
        }
        Value::Mix(m, _) => {
            let mut keys: Vec<(&String, &f64)> = m.iter().collect();
            keys.sort_by_key(|(k, _)| (*k).clone());
            if method == "raku" || method == "perl" {
                let pairs = keys
                    .iter()
                    .map(|(k, v)| {
                        let v_str = if v.fract() == 0.0 {
                            format!("{}", **v as i64)
                        } else {
                            format!("{}", v)
                        };
                        format!(
                            "\"{}\"=>{}",
                            k.replace('\\', "\\\\").replace('"', "\\\""),
                            v_str
                        )
                    })
                    .collect::<Vec<_>>()
                    .join(",");
                Some(Ok(Value::str(format!("({}).Mix", pairs))))
            } else {
                // gist: Mix(key(weight) ...)
                let inner = keys
                    .iter()
                    .map(|(k, v)| {
                        if (**v - 1.0).abs() < f64::EPSILON {
                            (*k).clone()
                        } else if v.fract() == 0.0 {
                            format!("{}({})", k, **v as i64)
                        } else {
                            format!("{}({})", k, v)
                        }
                    })
                    .collect::<Vec<_>>()
                    .join(" ");
                Some(Ok(Value::str(format!("Mix({})", inner))))
            }
        }
        Value::Package(name) => {
            if method == "gist" {
                let full = name.resolve();
                let short = full.rsplit("::").next().unwrap_or(&full);
                Some(Ok(Value::str(format!("({})", short))))
            } else {
                // .raku returns the full type name
                Some(Ok(Value::str(name.resolve())))
            }
        }
        Value::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "Stash" && (method == "gist" || method == "Str") => {
            // Stash.gist and Stash.Str return the package name
            if let Some(Value::Str(name)) = attributes.get("name") {
                Some(Ok(Value::str(name.to_string())))
            } else {
                Some(Ok(Value::str(String::new())))
            }
        }
        Value::Instance { .. } | Value::Enum { .. } => None,
        Value::Version {
            parts, plus, minus, ..
        } => {
            let s = Value::version_parts_to_string(parts);
            let suffix = if *plus {
                "+"
            } else if *minus {
                "-"
            } else {
                ""
            };
            Some(Ok(Value::str(format!("v{}{}", s, suffix))))
        }
        Value::Str(s) => {
            if method == "raku" || method == "perl" {
                // .raku wraps strings in quotes and escapes special chars
                let escaped = s
                    .replace('\\', "\\\\")
                    .replace('"', "\\\"")
                    .replace('$', "\\$")
                    .replace('@', "\\@")
                    .replace('%', "\\%")
                    .replace('&', "\\&")
                    .replace('{', "\\{")
                    .replace('\n', "\\n")
                    .replace('\t', "\\t")
                    .replace('\r', "\\r")
                    .replace('\0', "\\0");
                Some(Ok(Value::str(format!("\"{}\"", escaped))))
            } else {
                Some(Ok(Value::Str(s.clone())))
            }
        }
        Value::Array(_, _) if method == "raku" || method == "perl" => {
            Some(Ok(Value::str(raku_value(target))))
        }
        Value::Seq(_) if method == "raku" || method == "perl" => {
            Some(Ok(Value::str(raku_value(target))))
        }
        Value::Array(items, kind) if method == "gist" => {
            fn gist_item(v: &Value) -> String {
                match v {
                    Value::Nil => "Nil".to_string(),
                    Value::Array(inner, kind) => {
                        let elems = inner.iter().map(gist_item).collect::<Vec<_>>().join(" ");
                        gist_array_wrap(&elems, *kind)
                    }
                    Value::Seq(inner) | Value::Slip(inner) => {
                        let elems = inner.iter().map(gist_item).collect::<Vec<_>>().join(" ");
                        format!("({})", elems)
                    }
                    Value::Hash(map) => {
                        let mut sorted_keys: Vec<&String> = map.keys().collect();
                        sorted_keys.sort();
                        let parts: Vec<String> = sorted_keys
                            .iter()
                            .map(|k| format!("{} => {}", k, gist_item(&map[*k])))
                            .collect();
                        format!("{{{}}}", parts.join(" "))
                    }
                    other if other.is_range() => range_gist_string(other),
                    other => other.to_string_value(),
                }
            }
            // Shaped arrays: format with newlines between rows
            if *kind == crate::value::ArrayKind::Shaped
                && items.iter().any(|v| matches!(v, Value::Array(..)))
            {
                let rows: Vec<String> = items.iter().map(gist_item).collect();
                let inner = rows.join("\n ");
                return Some(Some(Ok(Value::str(format!("[{}]", inner)))));
            }
            let inner = items.iter().map(gist_item).collect::<Vec<_>>().join(" ");
            Some(Ok(Value::str(gist_array_wrap(&inner, *kind))))
        }
        Value::Seq(items) | Value::Slip(items) if method == "gist" => {
            fn gist_item(v: &Value) -> String {
                match v {
                    Value::Nil => "Nil".to_string(),
                    Value::Array(inner, kind) => {
                        let elems = inner.iter().map(gist_item).collect::<Vec<_>>().join(" ");
                        gist_array_wrap(&elems, *kind)
                    }
                    Value::Seq(inner) | Value::Slip(inner) => {
                        let elems = inner.iter().map(gist_item).collect::<Vec<_>>().join(" ");
                        format!("({})", elems)
                    }
                    Value::Hash(map) => {
                        let mut sorted_keys: Vec<&String> = map.keys().collect();
                        sorted_keys.sort();
                        let parts: Vec<String> = sorted_keys
                            .iter()
                            .map(|k| format!("{} => {}", k, gist_item(&map[*k])))
                            .collect();
                        format!("{{{}}}", parts.join(" "))
                    }
                    other if other.is_range() => range_gist_string(other),
                    other => other.to_string_value(),
                }
            }
            let inner = items.iter().map(gist_item).collect::<Vec<_>>().join(" ");
            Some(Ok(Value::str(format!("({})", inner))))
        }
        Value::Slip(items) if method == "raku" || method == "perl" => {
            let inner = items.iter().map(raku_value).collect::<Vec<_>>().join(", ");
            Some(Ok(Value::str(format!("slip({})", inner))))
        }
        Value::Junction { .. } if method == "raku" || method == "perl" => None,
        // Sub/Routine/WeakSub: delegate to interpreter for proper raku/gist/Str
        Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. } => None,
        Value::Pair(k, v) => {
            if method == "raku" || method == "perl" {
                Some(Ok(Value::str(raku_value(target))))
            } else {
                // gist: use " => " separator
                Some(Ok(Value::str(format!(
                    "{} => {}",
                    k,
                    runtime::gist_value(v)
                ))))
            }
        }
        Value::ValuePair(k, v) => {
            if method == "raku" || method == "perl" {
                Some(Ok(Value::str(raku_value(target))))
            } else {
                // gist: use " => " separator
                Some(Ok(Value::str(format!(
                    "{} => {}",
                    runtime::gist_value(k),
                    runtime::gist_value(v)
                ))))
            }
        }
        Value::BigInt(i) => {
            if method == "raku" || method == "perl" {
                Some(Ok(Value::str(format!("{i}.0"))))
            } else {
                Some(Ok(Value::str(i.to_string())))
            }
        }
        Value::Int(i) => Some(Ok(Value::str(format!("{}", i)))),
        Value::Num(f) => {
            if method == "raku" || method == "perl" {
                Some(Ok(Value::str(raku_value(target))))
            } else {
                // gist
                Some(Ok(Value::str(format!("{}", f))))
            }
        }
        Value::Complex(r, i) => {
            if method == "raku" || method == "perl" {
                Some(Ok(Value::str(format!(
                    "<{}>",
                    crate::value::format_complex(*r, *i)
                ))))
            } else {
                Some(Ok(Value::str(crate::value::format_complex(*r, *i))))
            }
        }
        Value::Hash(map) => {
            if method == "raku" || method == "perl" {
                // Delegate to raku_value which has cycle detection for
                // self-referencing hashes (e.g. %h<b> = %h).
                Some(Ok(Value::str(raku_value(target))))
            } else {
                // gist
                let mut sorted_keys: Vec<&String> = map.keys().collect();
                sorted_keys.sort();
                let parts: Vec<String> = sorted_keys
                    .iter()
                    .map(|k| format!("{} => {}", k, runtime::gist_value(&map[*k])))
                    .collect();
                Some(Ok(Value::str(format!("{{{}}}", parts.join(", ")))))
            }
        }
        _ if target.is_range() && (method == "gist" || method == "raku" || method == "perl") => {
            Some(Ok(Value::str(if method == "gist" {
                range_gist_string(target)
            } else {
                raku_value(target)
            })))
        }
        Value::LazyList(_) => None, // fall through to runtime to force
        _ => Some(Ok(Value::str(target.to_string_value()))),
    })
}
