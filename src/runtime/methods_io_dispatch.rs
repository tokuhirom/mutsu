use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    pub(super) fn dispatch_say(&mut self, target: &Value) -> Result<Value, RuntimeError> {
        let gist = self.render_gist_value(target);
        self.write_to_named_handle("$*OUT", &gist, true)?;
        Ok(Value::Bool(true))
    }

    pub(super) fn dispatch_print(&mut self, target: &Value) -> Result<Value, RuntimeError> {
        self.write_to_named_handle("$*OUT", &target.to_string_value(), false)?;
        Ok(Value::Bool(true))
    }

    pub(super) fn dispatch_put(&mut self, target: &Value) -> Result<Value, RuntimeError> {
        self.write_to_named_handle("$*OUT", &target.to_string_value(), true)?;
        Ok(Value::Bool(true))
    }

    pub(super) fn dispatch_note(&mut self, target: &Value) -> Result<Value, RuntimeError> {
        let content = format!("{}\n", self.render_gist_value(target));
        self.write_to_named_handle("$*ERR", &content, false)?;
        Ok(Value::Nil)
    }

    pub(super) fn dispatch_encode(
        &mut self,
        target: &Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        // Extract the first positional (non-Pair) argument as the encoding name
        let encoding = args
            .iter()
            .find(|v| !matches!(v, Value::Pair(..)))
            .map(|v| v.to_string_value())
            .unwrap_or_else(|| "utf-8".to_string());
        let replacement = Self::named_value(args, "replacement").map(|v| v.to_string_value());
        let normalized_encoding = self
            .find_encoding(&encoding)
            .map(|e| e.name.as_str().to_lowercase())
            .unwrap_or_else(|| encoding.to_lowercase());
        let input = target.to_string_value();
        let translated = self.translate_newlines_for_encode(&input);
        let mut attrs = HashMap::new();
        let type_name = match normalized_encoding.as_str() {
            "utf-16" | "utf16" => {
                let units: Vec<Value> = translated
                    .encode_utf16()
                    .map(|u| Value::Int(u as i64))
                    .collect();
                attrs.insert("bytes".to_string(), Value::array(units));
                "utf16"
            }
            _ => {
                let bytes = self.encode_with_encoding_and_replacement(
                    &translated,
                    &encoding,
                    replacement.as_deref(),
                )?;
                let bytes_vals: Vec<Value> =
                    bytes.into_iter().map(|b| Value::Int(b as i64)).collect();
                attrs.insert("bytes".to_string(), Value::array(bytes_vals));
                match normalized_encoding.as_str() {
                    "utf-8" | "utf8" => "utf8",
                    _ => "Buf",
                }
            }
        };
        Ok(Value::make_instance(Symbol::intern(type_name), attrs))
    }

    pub(super) fn dispatch_decode(
        &mut self,
        target: &Value,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if let Value::Instance {
            class_name,
            attributes,
            ..
        } = target
            && crate::runtime::utils::is_buf_or_blob_class(&class_name.resolve())
        {
            let default_encoding = if class_name == "utf16" {
                "utf-16"
            } else {
                "utf-8"
            };
            let encoding = args
                .first()
                .map(|v| v.to_string_value())
                .unwrap_or_else(|| default_encoding.to_string());
            let normalized_encoding = self
                .find_encoding(&encoding)
                .map(|e| e.name.as_str().to_lowercase())
                .unwrap_or_else(|| encoding.to_lowercase());
            let bytes = if class_name == "utf16" {
                if let Some(Value::Array(items, ..)) = attributes.get("bytes") {
                    let use_be = normalized_encoding == "utf-16be";
                    let mut out = Vec::with_capacity(items.len() * 2);
                    for item in items.iter() {
                        let unit = match item {
                            Value::Int(i) => *i as u16,
                            _ => 0u16,
                        };
                        let pair = if use_be {
                            unit.to_be_bytes()
                        } else {
                            unit.to_le_bytes()
                        };
                        out.extend_from_slice(&pair);
                    }
                    out
                } else {
                    Vec::new()
                }
            } else if let Some(Value::Array(items, ..)) = attributes.get("bytes") {
                items
                    .iter()
                    .map(|v| match v {
                        Value::Int(i) => *i as u8,
                        _ => 0,
                    })
                    .collect()
            } else {
                Vec::new()
            };
            let decoded = match self.decode_with_encoding(&bytes, &encoding) {
                Ok(d) => d,
                Err(e) => return Some(Err(e)),
            };
            let normalized = self.translate_newlines_for_decode(&decoded);
            return Some(Ok(Value::str(normalized)));
        }
        None
    }

    pub(super) fn dispatch_subbuf(
        &self,
        target: &Value,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if let Value::Instance {
            class_name,
            attributes,
            ..
        } = target
            && (class_name == "Buf" || class_name == "Blob")
        {
            let bytes = if let Some(Value::Array(items, ..)) = attributes.get("bytes") {
                items.to_vec()
            } else {
                Vec::new()
            };
            let len = bytes.len();
            let start_raw = args
                .first()
                .map(|v| match v {
                    Value::Int(i) => *i,
                    Value::Num(n) => *n as i64,
                    other => other.to_f64() as i64,
                })
                .unwrap_or(0);
            let start = if start_raw < 0 {
                len.saturating_sub(start_raw.unsigned_abs() as usize)
            } else {
                (start_raw as usize).min(len)
            };
            let end = if let Some(length_raw) = args.get(1).map(|v| match v {
                Value::Int(i) => *i,
                Value::Num(n) => *n as i64,
                other => other.to_f64() as i64,
            }) {
                if length_raw <= 0 {
                    start
                } else {
                    start.saturating_add(length_raw as usize).min(len)
                }
            } else {
                len
            };
            let mut attrs = HashMap::new();
            attrs.insert(
                "bytes".to_string(),
                Value::array(bytes[start..end].to_vec()),
            );
            return Some(Ok(Value::make_instance(*class_name, attrs)));
        }
        None
    }

    pub(super) fn dispatch_shape(&self, target: &Value) -> Option<Result<Value, RuntimeError>> {
        if let Value::Array(_, kind) = target {
            if *kind == crate::value::ArrayKind::Shaped
                && let Some(shape) = Self::infer_array_shape(target)
            {
                return Some(Ok(Value::array(
                    shape.into_iter().map(|n| Value::Int(n as i64)).collect(),
                )));
            }
            // Unshaped arrays return (*,)
            return Some(Ok(Value::array(vec![Value::Whatever])));
        }
        None
    }

    pub(super) fn dispatch_default(target: &Value) -> Option<Result<Value, RuntimeError>> {
        if matches!(target, Value::Array(..)) {
            return Some(Ok(Value::Package(Symbol::intern("Any"))));
        }
        if matches!(target, Value::Set(_, _)) {
            return Some(Ok(Value::Bool(false)));
        }
        if matches!(target, Value::Bag(_, _)) {
            return Some(Ok(Value::Int(0)));
        }
        if matches!(target, Value::Mix(_, _)) {
            return Some(Ok(Value::Num(0.0)));
        }
        None
    }
}
