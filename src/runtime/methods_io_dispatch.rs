use super::*;
use crate::symbol::Symbol;
use crate::value::ValueView;

impl Interpreter {
    /// Interpreter-native `.encode` (a `Cool` scalar -> `Buf`) and `.decode`
    /// (a `Buf`/`Blob` -> `Str`) for built-in receivers, sharing the single
    /// `dispatch_encode` / `dispatch_decode` impl the interpreter's catch-all also
    /// uses (ledger §D). Pure transformations: they stringify/encode or decode
    /// bytes via the VM-owned encoding registry (`&self` reads) and allocate no
    /// `io_handles`.
    ///
    /// `.encode` is gated to plain `Cool` scalars (Str / numeric / Bool); `.decode`
    /// is gated by `dispatch_decode` itself to `Buf`/`Blob` instances (it returns
    /// `None` for anything else). User Instances (a custom `.encode`/`.decode` is
    /// resolved earlier as a compiled method), `Supply` (its own chunk-encode), and
    /// `Buf` receivers for `.encode` fall through to the interpreter. The 0-arg
    /// `.encode` is already native via `methods_0arg`; this drains the
    /// explicit-encoding form (`.encode("utf-16")`). Behavior-invariant.
    pub(crate) fn try_native_encode_decode(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        match method {
            "encode"
                if matches!(
                    target.view(),
                    ValueView::Str(_)
                        | ValueView::Int(_)
                        | ValueView::BigInt(_)
                        | ValueView::Num(_)
                        | ValueView::Rat(..)
                        | ValueView::FatRat(..)
                        | ValueView::Complex(..)
                        | ValueView::Bool(_)
                ) =>
            {
                Some(self.dispatch_encode(target, args))
            }
            // `dispatch_decode` is internally gated to Buf/Blob instances and
            // returns `None` for any other receiver, so it falls through cleanly.
            "decode" => self.dispatch_decode(target, args),
            _ => None,
        }
    }

    pub(super) fn dispatch_say(&mut self, target: &Value) -> Result<Value, RuntimeError> {
        let gist = self.render_gist_value(target)?;
        self.write_to_named_handle("$*OUT", &gist, true)?;
        Ok(Value::TRUE)
    }

    pub(super) fn dispatch_print(&mut self, target: &Value) -> Result<Value, RuntimeError> {
        let content = self.render_str_value(target);
        self.write_to_named_handle("$*OUT", &content, false)?;
        Ok(Value::TRUE)
    }

    pub(super) fn dispatch_printf(&mut self, target: &Value) -> Result<Value, RuntimeError> {
        let content = self.render_str_value(target);
        self.write_to_named_handle("$*OUT", &content, false)?;
        Ok(Value::TRUE)
    }

    pub(super) fn dispatch_sprintf(&mut self, target: &Value) -> Result<Value, RuntimeError> {
        // No-arg method form: `$format.sprintf`. The stringified invocant is the
        // format, consumed against zero arguments — so a format with any directive
        // (`"%s".sprintf`) is an arg-count mismatch, exactly as `sprintf("%s")` is,
        // while a directive-free string (`"no dir".sprintf`, `42.sprintf`) renders
        // as itself.
        let content = self.render_str_value(target);
        crate::runtime::sprintf::validate_sprintf_directives(&content, 0)?;
        // Still run the formatter so an escaped `%%` collapses to a literal `%`
        // (`"100%%".sprintf` is `100%`), matching `sprintf("100%%")`.
        Ok(Value::str(crate::runtime::format_sprintf_args(
            &content,
            &[],
        )))
    }

    pub(super) fn dispatch_put(&mut self, target: &Value) -> Result<Value, RuntimeError> {
        self.write_to_named_handle("$*OUT", &target.to_string_value(), true)?;
        Ok(Value::TRUE)
    }

    pub(super) fn dispatch_note(&mut self, target: &Value) -> Result<Value, RuntimeError> {
        let content = format!("{}\n", self.render_gist_value(target)?);
        self.write_to_named_handle("$*ERR", &content, false)?;
        // `note` returns True (like the `note LIST` sub form), not Nil.
        Ok(Value::TRUE)
    }

    /// Apply a Unicode normalization form before encoding. Accepts the short
    /// names (`C`/`D`/`KC`/`KD`) and the full `NF*` names; an unrecognized value
    /// is left as-is (encode as written), so a non-form second positional never
    /// changes the bytes.
    fn normalize_for_encode(s: &str, form: &str) -> String {
        use unicode_normalization::UnicodeNormalization;
        match form {
            "C" | "NFC" => s.nfc().collect(),
            "D" | "NFD" => s.nfd().collect(),
            "KC" | "NFKC" => s.nfkc().collect(),
            "KD" | "NFKD" => s.nfkd().collect(),
            _ => s.to_string(),
        }
    }

    pub(super) fn dispatch_encode(
        &mut self,
        target: &Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        // Positional args: [0] = encoding name, [1] = Unicode normalization form
        // (`C`/`D`/`KC`/`KD` i.e. NFC/NFD/NFKC/NFKD). Rakudo `#?rakudo skip`s the
        // normalization-form parameter ("We do not handle NDF yet"); mutsu
        // applies it (roast S32-str/encode.t "encoding to UTF-8, with NFD").
        let positionals: Vec<&Value> = args
            .iter()
            .filter(|v| !matches!(v.view(), ValueView::Pair(..)))
            .collect();
        let encoding = positionals
            .first()
            .map(|v| v.to_string_value())
            .unwrap_or_else(|| "utf-8".to_string());
        let replacement = Self::named_value(args, "replacement").map(|v| {
            if matches!(v.view(), ValueView::Bool(true)) {
                "?".to_string()
            } else {
                v.to_string_value()
            }
        });
        let normalized_encoding = self
            .find_encoding(&encoding)
            .map(|e| e.name.as_str().to_lowercase())
            .unwrap_or_else(|| encoding.to_lowercase());
        let raw_input = target.to_string_value();
        let input = match positionals.get(1).map(|v| v.to_string_value()) {
            Some(nf) => Self::normalize_for_encode(&raw_input, &nf),
            None => raw_input,
        };
        let translated = self.translate_newlines_for_encode(&input);
        let mut attrs = HashMap::new();
        let type_name = match normalized_encoding.as_str() {
            "utf-16" | "utf16" => {
                let units: Vec<Value> = translated
                    .encode_utf16()
                    .map(|u| Value::int(u as i64))
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
                    bytes.into_iter().map(|b| Value::int(b as i64)).collect();
                attrs.insert("bytes".to_string(), Value::array(bytes_vals));
                match normalized_encoding.as_str() {
                    "utf-8" | "utf8" => "utf8",
                    _ => "Blob[uint8]",
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
        if let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = target.view()
            && crate::runtime::utils::is_buf_or_blob_class(&class_name.resolve())
        {
            let cn = class_name.resolve();
            let is_wide = cn == "utf16" || cn == "buf16" || cn == "Buf[uint16]";
            let default_encoding = if is_wide { "utf-16" } else { "utf-8" };
            let encoding = args
                .iter()
                .find(|v| !matches!(v.view(), ValueView::Pair(..)))
                .map(|v| v.to_string_value())
                .unwrap_or_else(|| default_encoding.to_string());
            let replacement = Self::named_value(args, "replacement").map(|v| {
                if matches!(v.view(), ValueView::Bool(true)) {
                    "\u{FFFD}".to_string()
                } else {
                    v.to_string_value()
                }
            });
            let normalized_encoding = self
                .find_encoding(&encoding)
                .map(|e| e.name.as_str().to_lowercase())
                .unwrap_or_else(|| encoding.to_lowercase());
            let bytes = if is_wide {
                if let Some(ValueView::Array(items, ..)) =
                    attributes.as_map().get("bytes").map(|v| v.view())
                {
                    let use_be = normalized_encoding == "utf-16be";
                    let mut out = Vec::with_capacity(items.len() * 2);
                    for item in items.iter() {
                        let unit = match item.view() {
                            ValueView::Int(i) => i as u16,
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
            } else if let Some(ValueView::Array(items, ..)) =
                attributes.as_map().get("bytes").map(|v| v.view())
            {
                items
                    .iter()
                    .map(|v| match v.view() {
                        ValueView::Int(i) => i as u8,
                        _ => 0,
                    })
                    .collect()
            } else {
                Vec::new()
            };
            let decoded = match self.decode_with_encoding_and_replacement(
                &bytes,
                &encoding,
                replacement.as_deref(),
            ) {
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
        if let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = target.view()
            && (class_name == "Buf" || class_name == "Blob")
        {
            let bytes = if let Some(ValueView::Array(items, ..)) =
                attributes.as_map().get("bytes").map(|v| v.view())
            {
                items.to_vec()
            } else {
                Vec::new()
            };
            let len = bytes.len();
            let start_raw = args
                .first()
                .map(|v| match v.view() {
                    ValueView::Int(i) => i,
                    ValueView::Num(n) => n as i64,
                    _ => v.to_f64() as i64,
                })
                .unwrap_or(0);
            let start = if start_raw < 0 {
                len.saturating_sub(start_raw.unsigned_abs() as usize)
            } else {
                (start_raw as usize).min(len)
            };
            let end = if let Some(length_raw) = args.get(1).map(|v| match v.view() {
                ValueView::Int(i) => i,
                ValueView::Num(n) => n as i64,
                _ => v.to_f64() as i64,
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
            return Some(Ok(Value::make_instance(class_name, attrs)));
        }
        None
    }

    pub(super) fn dispatch_shape(&self, target: &Value) -> Option<Result<Value, RuntimeError>> {
        if let ValueView::Array(_, kind) = target.view() {
            if kind == crate::value::ArrayKind::Shaped
                && let Some(shape) = Self::infer_array_shape(target)
            {
                return Some(Ok(Value::array(
                    shape.into_iter().map(|n| Value::int(n as i64)).collect(),
                )));
            }
            // Unshaped arrays return (*,)
            return Some(Ok(Value::array(vec![Value::WHATEVER])));
        }
        None
    }

    /// The value-carried `is default(...)` of a Hash/Array, if any. Embedded in
    /// `HashData`/`ArrayData` so it travels with the value through copy-on-write,
    /// raw-parameter binding, and list construction — unlike the name-keyed
    /// `var_defaults` table, which only resolves for a value still held under its
    /// original variable name.
    pub(crate) fn value_carried_default(target: &Value) -> Option<Value> {
        match target.view() {
            ValueView::Hash(h) => h.default.as_deref().cloned(),
            ValueView::Array(a, _) => a.default.as_deref().cloned(),
            _ => None,
        }
    }

    pub(super) fn dispatch_default(target: &Value) -> Option<Result<Value, RuntimeError>> {
        if let Some(def) = Self::value_carried_default(target) {
            return Some(Ok(def));
        }
        if matches!(target.view(), ValueView::Array(..)) {
            return Some(Ok(Value::package(Symbol::intern("Any"))));
        }
        if matches!(target.view(), ValueView::Set(_, _)) {
            return Some(Ok(Value::FALSE));
        }
        if matches!(target.view(), ValueView::Bag(_, _)) {
            return Some(Ok(Value::int(0)));
        }
        if matches!(target.view(), ValueView::Mix(_, _)) {
            return Some(Ok(Value::num(0.0)));
        }
        None
    }
}
