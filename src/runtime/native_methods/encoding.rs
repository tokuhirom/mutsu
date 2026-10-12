use crate::runtime::*;
use crate::symbol::Symbol;
use crate::value::ValueView;
use crate::value::AttrMap;

impl Interpreter {
    pub(in crate::runtime) fn native_encoding_builtin(
        attributes: &AttrMap,
        method: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        Ok(match method {
            "name" => attributes
                .get("name")
                .cloned()
                .unwrap_or(Value::str(String::new())),
            "alternative-names" => attributes
                .get("alternative-names")
                .cloned()
                .unwrap_or_else(|| Value::array(Vec::new())),
            "encoder" => {
                let enc_name = attributes
                    .get("name")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let mut attrs = HashMap::new();
                attrs.insert("encoding".to_string(), Value::str(enc_name));
                // Extract :replacement named arg from args
                for arg in args {
                    if let ValueView::Pair(key, value) = arg.view()
                        && key == "replacement"
                    {
                        attrs.insert("replacement".to_string(), value.clone());
                    }
                }
                Value::make_instance(Symbol::intern("Encoding::Encoder::Builtin"), attrs)
            }
            // Cost: O(1).
            "decoder" => {
                let enc_name = attributes
                    .get("name")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let translate_nl = args.iter().any(|arg| {
                    matches!(arg.view(), ValueView::Pair(key, value)
                        if key == "translate-nl" && value.truthy())
                });
                return crate::runtime::stream_decoder_object::new_decoder(&enc_name, translate_nl);
            }
            "gist" | "Str" => {
                let name = attributes
                    .get("name")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                Value::str(format!("Encoding::Builtin<{}>", name))
            }
            "WHAT" => Value::package(Symbol::intern("Encoding::Builtin")),
            _ => Value::NIL,
        })
    }

    pub(in crate::runtime) fn native_encoding_encoder(
        attributes: &AttrMap,
        method: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        match method {
            "encode-chars" => {
                let input = args
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let enc_name = attributes
                    .get("encoding")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let replacement = attributes.get("replacement");
                let enc_lower = enc_name.to_lowercase();
                let is_ascii = matches!(enc_lower.as_str(), "ascii" | "us-ascii");
                let is_utf8_c8 = enc_lower == "utf8-c8";

                let mut bytes: Vec<Value> = Vec::new();
                if is_utf8_c8 {
                    for b in crate::runtime::utf8_c8::encode_utf8_c8(&input) {
                        bytes.push(Value::int(b as i64));
                    }
                } else if is_ascii {
                    for ch in input.chars() {
                        if ch as u32 > 127 {
                            if let Some(repl) = replacement {
                                let repl_str = if matches!(repl.view(), ValueView::Bool(true)) {
                                    // :replacement (Bool True) -> default replacement char '?'
                                    "?".to_string()
                                } else {
                                    repl.to_string_value()
                                };
                                for b in repl_str.bytes() {
                                    bytes.push(Value::int(b as i64));
                                }
                            } else {
                                return Err(RuntimeError::new(format!(
                                    "Cannot encode character '{}' (U+{:04X}) in ASCII",
                                    ch, ch as u32
                                )));
                            }
                        } else {
                            bytes.push(Value::int(ch as u32 as i64));
                        }
                    }
                } else {
                    // UTF-8 encoding
                    for b in input.as_bytes() {
                        bytes.push(Value::int(*b as i64));
                    }
                }

                Ok(crate::value::value_buf::make_buf(
                    Symbol::intern("Blob[uint8]"),
                    bytes,
                ))
            }
            "WHAT" => Ok(Value::package(Symbol::intern("Encoding::Encoder::Builtin"))),
            _ => Ok(Value::NIL),
        }
    }
}
