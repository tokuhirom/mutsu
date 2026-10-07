use super::*;

impl Interpreter {
    // Cost: TODO
    pub(crate) fn io_handle_print_nl(
        &mut self,
        target_val: &Value,
        _args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let nl = self.with_handle_mut(target_val, |state| Ok(state.nl_out.clone()))?;
        self.write_to_handle_value(target_val, &nl, false)?;
        Ok(Value::TRUE)
    }

    // Cost: TODO
    pub(crate) fn io_handle_write(
        &mut self,
        target_val: &Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let mut bytes = Vec::new();
        for arg in args {
            match arg.view() {
                ValueView::Instance { class_name, .. }
                    if {
                        let cn = class_name.resolve();
                        crate::runtime::utils::is_buf_or_blob_class(&cn)
                    } =>
                {
                    bytes.extend(self.supply_chunk_to_bytes(arg, "utf-8"));
                }
                _ => bytes.extend(self.render_str_value(arg).into_bytes()),
            }
        }
        // Write raw bytes directly to avoid UTF-8 lossy conversion
        // which corrupts non-UTF-8 binary data (e.g., ISO-8859-1 encoded bytes)
        self.write_bytes_to_handle_value(target_val, &bytes)?;
        Ok(Value::TRUE)
    }

    /// `IO::Handle.WRITE(Blob:D $buf)`: Rakudo's primitive write, the raw bytes
    /// of `$buf` to the handle. Anything but a `Blob` fails the bind.
    // Cost: O(b), b = bytes written.
    pub(crate) fn io_handle_write_primitive(
        &mut self,
        target_val: &Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let Some(buf) = args.first() else {
            return Err(
                crate::runtime::methods_signature_errors::make_multi_no_match_error("WRITE"),
            );
        };
        let is_blob = matches!(buf.view(), ValueView::Instance { class_name, .. }
            if crate::runtime::utils::is_buf_or_blob_class(&class_name.resolve()));
        if !is_blob {
            return Err(crate::runtime::utils::typecheck_binding_parameter_with_hint(
                "$buf",
                "Blob",
                buf,
                &crate::runtime::utils::value_short_repr(buf),
                None,
            ));
        }
        let bytes = self.supply_chunk_to_bytes(buf, "utf-8");
        self.write_bytes_to_handle_value(target_val, &bytes)?;
        Ok(Value::TRUE)
    }

    // Cost: TODO
    pub(crate) fn io_handle_print(
        &mut self,
        target_val: &Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let mut content = String::new();
        for arg in args {
            content.push_str(&self.render_str_value(arg));
        }
        self.write_to_handle_value_trying(target_val, &content, false, "print")?;
        Ok(Value::TRUE)
    }

    // Cost: TODO
    pub(crate) fn io_handle_printf(
        &mut self,
        target_val: &Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        // printf requires a format argument; a bare `$handle.printf`
        // matches no candidate (roast .../multi-no-match.t).
        if args.is_empty() {
            return Err(
                crate::runtime::methods_signature_errors::make_multi_no_match_error(
                    "printf",
                ),
            );
        }
        // If the first arg is a Junction, thread through it
        if let Some(ValueView::Junction { kind: _, values }) = args.first().map(Value::view)
        {
            let mut content = String::new();
            for v in values.iter() {
                content.push_str(&self.render_str_value(v));
            }
            self.write_to_handle_value_trying(target_val, &content, false, "printf")?;
            Ok(Value::TRUE)
        } else {
            let fmt = args
                .first()
                .map(|v| v.to_string_value())
                .unwrap_or_default();
            let rest = &args[1..];
            super::sprintf::validate_sprintf_directives(&fmt, rest.len())?;
            let content = super::sprintf::format_sprintf_args(&fmt, rest);
            self.write_to_handle_value_trying(target_val, &content, false, "printf")?;
            Ok(Value::TRUE)
        }
    }

    // Cost: TODO
    pub(crate) fn io_handle_say(
        &mut self,
        target_val: &Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let mut content = String::new();
        for arg in args {
            content.push_str(&self.render_gist_value(arg)?);
        }
        self.write_to_handle_value_trying(target_val, &content, true, "say")?;
        Ok(Value::TRUE)
    }

    // Cost: TODO
    pub(crate) fn io_handle_put(
        &mut self,
        target_val: &Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let mut content = String::new();
        for arg in args {
            content.push_str(&self.render_str_value(arg));
        }
        self.write_to_handle_value_trying(target_val, &content, true, "put")?;
        Ok(Value::TRUE)
    }

    // Cost: TODO
    pub(crate) fn io_handle_spurt(
        &mut self,
        target_val: &Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        // IO::Handle.spurt($data) -- write data to the handle
        let content_value = args.first().cloned().unwrap_or(Value::str(String::new()));
        let is_buf = crate::runtime::Interpreter::is_buf_value(&content_value);
        if is_buf {
            let bytes = crate::runtime::Interpreter::extract_buf_bytes(&content_value);
            self.write_bytes_to_handle_value(target_val, &bytes)?;
        } else {
            let content = content_value.to_string_value();
            // Determine encoding from the handle's state
            let enc = self
                .with_handle_mut_opt(target_val, |state| Ok(state.encoding.clone()))?
                .unwrap_or_else(|| "utf-8".to_string());
            let bytes = if enc == "utf-8" || enc == "utf8" {
                content.into_bytes()
            } else {
                self.encode_with_encoding(&content, &enc)?
            };
            self.write_bytes_to_handle_value(target_val, &bytes)?;
        }
        Ok(Value::TRUE)
    }

}
