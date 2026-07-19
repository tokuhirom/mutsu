/// Unicode and character methods: bytes, decode, chars, ord, ords, uniprop, uniname,
/// uninames, uniparse, uniprops, unival, univals, chr, chrs
use crate::value::{RuntimeError, Value, ValueView};
use unicode_normalization::UnicodeNormalization;
use unicode_segmentation::UnicodeSegmentation;

use super::make_no_match_error;

pub(super) fn dispatch(
    target: &Value,
    method: &str,
) -> Option<Option<Result<Value, RuntimeError>>> {
    match method {
        "bytes" => Some(match target.view() {
            ValueView::Instance {
                class_name,
                attributes,
                ..
            } if {
                let cn = class_name.resolve();
                cn == "Buf"
                    || cn == "Blob"
                    || cn == "utf8"
                    || cn == "utf16"
                    || cn.starts_with("Buf[")
                    || cn.starts_with("Blob[")
                    || cn.starts_with("buf")
                    || cn.starts_with("blob")
            } =>
            {
                let elems = if let Some(ValueView::Array(bytes, ..)) =
                    attributes.as_map().get("bytes").map(Value::view)
                {
                    bytes.len() as i64
                } else {
                    0
                };
                let cn = class_name.resolve();
                let bytes_per_elem: i64 = if cn.contains("16") {
                    2
                } else if cn.contains("32") {
                    4
                } else if cn.contains("64") {
                    8
                } else {
                    1
                };
                Some(Ok(Value::int(elems * bytes_per_elem)))
            }
            ValueView::Str(s) => Some(Ok(Value::int(s.len() as i64))),
            _ => Some(Ok(Value::int(target.to_string_value().len() as i64))),
        }),
        "decode" => Some(super::super::decode_buf_method(target, None)),
        "chars" => {
            // Buf/Blob instances: throw X::Buf::AsStr
            if let ValueView::Instance { class_name, .. } = target.view()
                && crate::runtime::utils::is_buf_or_blob_class(&class_name.resolve())
            {
                let msg = format!(
                    "Cannot use a {} as a string, but you called the .chars method on it",
                    class_name
                );
                let mut ex_attrs = std::collections::HashMap::new();
                ex_attrs.insert("message".to_string(), Value::str(msg.clone()));
                ex_attrs.insert("method".to_string(), Value::str("chars".to_string()));
                let exception =
                    Value::make_instance(crate::symbol::Symbol::intern("X::Buf::AsStr"), ex_attrs);
                let mut err = crate::value::RuntimeError::new(msg);
                err.exception = Some(Box::new(exception));
                return Some(Some(Err(err)));
            }
            Some(Some(Ok(Value::int(
                target.to_string_value().graphemes(true).count() as i64,
            ))))
        }
        "ord" => {
            let s = target.to_string_value();
            if let Some(ch) = s.chars().next() {
                Some(Some(Ok(Value::int(ch as u32 as i64))))
            } else {
                Some(Some(Ok(Value::NIL)))
            }
        }
        "ords" => {
            let s = target.to_string_value();
            let normalized: String = s.nfc().collect();
            let ords: Vec<Value> = normalized
                .chars()
                .map(|c| Value::int(c as u32 as i64))
                .collect();
            // `.ords` returns a Seq (like `.comb`), not a List.
            Some(Some(Ok(Value::seq(ords))))
        }
        "uniprop" => {
            match target.view() {
                ValueView::Package(_) => {
                    return Some(Some(Err(make_no_match_error("uniprop"))));
                }
                ValueView::Int(i) => {
                    let cp = i as u32;
                    return Some(Some(Ok(
                        crate::builtins::uniprop::unicode_property_value_for_codepoint(cp, None),
                    )));
                }
                _ => {}
            }
            let s = target.to_string_value();
            if s.is_empty() {
                return Some(Some(Ok(Value::NIL)));
            }
            let ch = s.chars().next().unwrap();
            Some(Some(Ok(Value::str(
                crate::builtins::unicode::unicode_general_category(ch),
            ))))
        }
        "uniname" => match target.view() {
            ValueView::Int(i) => match crate::builtins::unicode::uniname_from_int(i) {
                Ok(name) => Some(Some(Ok(Value::str(name)))),
                Err(e) => Some(Some(Err(e))),
            },
            ValueView::Package(_) => Some(Some(Err(make_no_match_error("uniname")))),
            _ => {
                let s = target.to_string_value();
                if s.is_empty() {
                    return Some(Some(Ok(Value::NIL)));
                }
                let ch = s.chars().next().unwrap();
                Some(Some(Ok(Value::str(
                    crate::builtins::unicode::unicode_char_name(ch),
                ))))
            }
        },
        "uninames" => {
            let s = target.to_string_value();
            let names: Vec<Value> = s
                .chars()
                .map(|ch| Value::str(crate::builtins::unicode::unicode_char_name(ch)))
                .collect();
            // `.uninames` returns a Seq in raku (matters for `.raku`/`.WHAT`).
            Some(Some(Ok(Value::seq(names))))
        }
        "uniparse" | "parse-names" => {
            let s = target.to_string_value();
            Some(Some(crate::builtins::functions::uniparse_impl(&s)))
        }
        "uniprops" => {
            let s = target.to_string_value();
            if s.is_empty() {
                return Some(Some(Ok(Value::seq(vec![]))));
            }
            let props: Vec<Value> = s
                .chars()
                .map(|ch| Value::str(crate::builtins::unicode::unicode_general_category(ch)))
                .collect();
            // `.uniprops` returns a Seq in raku.
            Some(Some(Ok(Value::seq(props))))
        }
        "unival" => {
            // Type objects should throw an error
            if matches!(
                target.view(),
                ValueView::Package(_) | ValueView::CustomType { .. }
            ) {
                return Some(Some(Err(RuntimeError::new(
                    "Invocant of method 'unival' must be an object instance, not a type object"
                        .to_string(),
                ))));
            }
            let ch = match target.view() {
                ValueView::Int(i) => char::from_u32(i as u32),
                _ => {
                    let s = target.to_string_value();
                    s.chars().next()
                }
            };
            let Some(ch) = ch else {
                return Some(Some(Ok(Value::NIL)));
            };
            if let Some((n, d)) = crate::builtins::unicode::unicode_rat_value(ch) {
                return Some(Some(Ok(crate::value::make_rat(n, d))));
            }
            if let Some(n) = crate::builtins::unicode::unicode_numeric_int_value(ch) {
                return Some(Some(Ok(Value::int(n))));
            }
            if let Some(n) = crate::builtins::unicode::unicode_decimal_digit_value(ch) {
                return Some(Some(Ok(Value::int(n as i64))));
            }
            Some(Some(Ok(Value::num(f64::NAN))))
        }
        "univals" => {
            // Type objects should throw an error
            if matches!(
                target.view(),
                ValueView::Package(_) | ValueView::CustomType { .. }
            ) {
                return Some(Some(Err(RuntimeError::new(
                    "Invocant of method 'univals' must be an object instance, not a type object"
                        .to_string(),
                ))));
            }
            let s = match target.view() {
                ValueView::Int(i) => {
                    if let Some(ch) = char::from_u32(i as u32) {
                        ch.to_string()
                    } else {
                        return Some(Some(Ok(Value::seq(Vec::new()))));
                    }
                }
                _ => target.to_string_value(),
            };
            if s.is_empty() {
                return Some(Some(Ok(Value::seq(Vec::new()))));
            }
            let mut result = Vec::new();
            for ch in s.chars() {
                if let Some((n, d)) = crate::builtins::unicode::unicode_rat_value(ch) {
                    result.push(crate::value::make_rat(n, d));
                } else if let Some(n) = crate::builtins::unicode::unicode_numeric_int_value(ch) {
                    result.push(Value::int(n));
                } else if let Some(n) = crate::builtins::unicode::unicode_decimal_digit_value(ch) {
                    result.push(Value::int(n as i64));
                } else {
                    result.push(Value::num(f64::NAN));
                }
            }
            // `.univals` returns a Seq in raku.
            Some(Some(Ok(Value::seq(result))))
        }
        "chr" => {
            let (code, display) = match target.view() {
                ValueView::Int(i) => (i, format!("{}", i)),
                ValueView::BigInt(n) => {
                    // BigInt is always out of range for chr
                    let hex = format!("{:X}", &**n);
                    return Some(Some(Err(RuntimeError::new(format!(
                        "Codepoint {} (0x{}) is out of bounds in 'chr'",
                        *n, hex
                    )))));
                }
                ValueView::Num(f) => (f as i64, format!("{}", f as i64)),
                _ => {
                    let s = target.to_string_value();
                    let i = s.parse::<i64>().unwrap_or(0);
                    (i, format!("{}", i))
                }
            };
            if !(0..=0x10FFFF).contains(&code) {
                let hex = format!("{:X}", code);
                return Some(Some(Err(RuntimeError::new(format!(
                    "Codepoint {} (0x{}) is out of bounds in 'chr'",
                    display, hex
                )))));
            }
            if let Some(ch) = char::from_u32(code as u32) {
                // NFC-normalize: some codepoints decompose in NFC
                // (e.g., U+0F75 TIBETAN VOWEL SIGN UU -> U+0F71 + U+0F74)
                let s: String = ch.to_string().nfc().collect();
                Some(Some(Ok(Value::str(s))))
            } else {
                Some(Some(Err(RuntimeError::new(format!(
                    "Codepoint {} (0x{:X}) is out of bounds in 'chr'",
                    display, code
                )))))
            }
        }
        "chrs" => {
            // .chrs on a list/array of ints or a range
            let val_to_i64 = |v: &Value| -> i64 {
                match v.view() {
                    ValueView::Int(i) => i,
                    ValueView::Num(f) => f as i64,
                    _ => v.to_string_value().parse::<i64>().unwrap_or(0),
                }
            };
            let items: Vec<i64> = match target.view() {
                ValueView::Array(items, ..) => items.iter().map(&val_to_i64).collect(),
                ValueView::Seq(items) => items.iter().map(&val_to_i64).collect(),
                ValueView::Range(a, b) => (a..=b).collect(),
                ValueView::RangeExcl(a, b) => (a..b).collect(),
                _ => vec![val_to_i64(target)],
            };
            let s: String = items
                .iter()
                .filter_map(|&code| char::from_u32(code as u32))
                .collect();
            Some(Some(Ok(Value::str(s))))
        }
        _ => None,
    }
}
