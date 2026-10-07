use super::*;

impl Interpreter {
    // Cost: TODO
    pub(crate) fn io_handle_destroy(
        &mut self,
        target_val: &Value,
        _args: &[Value],
    ) -> Result<Value, RuntimeError> {
        // Standard handles ($*IN, $*OUT, $*ERR) must not be closed by DESTROY
        let is_std = if let Some(id) = Self::handle_id_from_value(target_val) {
            self.io_handles().map.get(&id).is_some_and(|s| {
                matches!(
                    s.target,
                    IoHandleTarget::Stdin | IoHandleTarget::Stdout | IoHandleTarget::Stderr
                )
            })
        } else {
            false
        };
        if !is_std {
            let _ = self.close_handle_value(target_val)?;
        }
        Ok(Value::TRUE)
    }

    // Cost: TODO
    pub(crate) fn io_handle_path(
        &mut self,
        target_val: &Value,
        _args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let target = Self::io_handle_attrs(target_val);
        // For standard handles ($*IN, $*OUT, $*ERR), return IO::Special
        if let Some(id) = Self::handle_id_from_value(target_val)
            && let Some(state) = self.io_handles().map.get(&id)
        {
            let special_name = match state.target {
                IoHandleTarget::Stdout => Some("STDOUT"),
                IoHandleTarget::Stderr => Some("STDERR"),
                IoHandleTarget::Stdin => Some("STDIN"),
                _ => None,
            };
            if let Some(name) = special_name {
                return Ok(Self::make_io_special_instance(name));
            }
        }
        if let Some(path_val) = target.get("path") {
            let io_path = match path_val.view() {
                ValueView::Instance { class_name, .. } if class_name == "IO::Path" => {
                    path_val.clone()
                }
                _ => self.make_io_path_instance(&path_val.to_string_value()),
            };
            return Ok(io_path);
        }
        Ok(Value::NIL)
    }

    // Cost: TODO
    pub(crate) fn io_handle_str(
        &mut self,
        target_val: &Value,
        _args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let target = Self::io_handle_attrs(target_val);
        if let Some(path_val) = target.get("path") {
            let path = match path_val.view() {
                ValueView::Instance {
                    class_name,
                    attributes,
                    ..
                } if class_name == "IO::Path" => attributes
                    .as_map()
                    .get("path")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default(),
                _ => path_val.to_string_value(),
            };
            return Ok(Value::str(path));
        }
        Ok(Value::str_from("IO::Handle()"))
    }

    // Rakudo: `IO::Handle<"path".IO>(opened|closed)` — distinct from
    // `.Str` (the bare path). The path is rendered via IO::Path.raku
    // (`"path".IO`); the open/closed suffix reflects the live state.
    // Cost: TODO
    pub(crate) fn io_handle_gist(
        &mut self,
        target_val: &Value,
        _args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let target = Self::io_handle_attrs(target_val);
        let opened = self
            .with_handle_mut(target_val, |state| Ok(state.is_opened()))
            .unwrap_or(false);
        let state = if opened { "opened" } else { "closed" };
        if let Some(path_val) = target.get("path") {
            let path_raku = match path_val.view() {
                ValueView::Package(name) => {
                    format!("({})", crate::qualified::unqualified_part(name).as_str())
                }
                ValueView::Instance { class_name, .. } if class_name == "IO::Path" => {
                    format!(
                        "{}.IO",
                        crate::builtins::methods_0arg::raku_repr::escape_raku_str(
                            &path_val.to_string_value()
                        )
                    )
                }
                _ => self
                    .call_method_with_values(path_val.clone(), "raku", vec![])
                    .map(|v| v.to_string_value())
                    .unwrap_or_else(|_| path_val.to_string_value()),
            };
            return Ok(Value::str(format!("IO::Handle<{path_raku}>({state})")));
        }
        Ok(Value::str(format!("IO::Handle<>({state})")))
    }

    // Cost: TODO
    pub(crate) fn io_handle_raku(
        &mut self,
        target_val: &Value,
        _args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let target = Self::io_handle_attrs(target_val);
        // Reconstructable but deliberately *unopened*: matches rakudo's
        // own `.raku` (no `handle`/`mode` shown), and lets `IO::CatHandle`
        // re-open this handle itself when it appears as one of its
        // sources (`cat_open_source` opens any un-opened `IO::Handle`
        // source via `.open(:r)` before reading it).
        let mut parts = Vec::new();
        if let Some(path_val) = target.get("path") {
            let path_raku = self
                .call_method_with_values(path_val.clone(), "raku", vec![])
                .map(|v| v.to_string_value())
                .unwrap_or_else(|_| path_val.to_string_value());
            parts.push(format!("path => {path_raku}"));
        }
        if let Some(chomp) = target.get("chomp") {
            parts.push(format!(
                "chomp => {}",
                if chomp.truthy() { "True" } else { "False" }
            ));
        }
        if let Some(nl_in) = target.get("nl-in") {
            let nl_in_raku = self
                .call_method_with_values(nl_in.clone(), "raku", vec![])
                .map(|v| v.to_string_value())
                .unwrap_or_else(|_| nl_in.to_string_value());
            parts.push(format!("nl-in => {nl_in_raku}"));
        }
        if let Some(nl_out) = target.get("nl-out") {
            parts.push(format!(
                "nl-out => {}",
                crate::builtins::methods_0arg::raku_repr::escape_raku_str(
                    &nl_out.to_string_value()
                )
            ));
        }
        if let Some(encoding) = target.get("encoding") {
            parts.push(format!(
                "encoding => {}",
                crate::builtins::methods_0arg::raku_repr::escape_raku_str(
                    &encoding.to_string_value()
                )
            ));
        }
        Ok(Value::str(format!("IO::Handle.new({})", parts.join(", "))))
    }

    // Cost: TODO
    pub(crate) fn io_handle_nl_out(
        &mut self,
        target_val: &Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        // A subclass that wraps a handle (`IO::MiddleMan`) has no handle of its
        // own: the getter answers its `nl-out` attribute, or the default.
        if args.is_empty() && Self::handle_id_from_value(target_val).is_none() {
            let attr = match target_val.view() {
                ValueView::Instance { attributes, .. } => {
                    attributes.as_map().get("nl-out").cloned()
                }
                _ => None,
            };
            return Ok(attr.unwrap_or_else(|| Value::str_from("\n")));
        }
        let set = args.first().map(|a| a.to_string_value());
        let nl = self.with_handle_mut(target_val, |state| Ok(state.nl_out_setting(set)))?;
        Ok(Value::str(nl))
    }

    // Cost: TODO
    pub(crate) fn io_handle_nl_in(
        &mut self,
        target_val: &Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if let Some(arg) = args.first() {
            let _ = self.with_handle_mut_opt(target_val, |state| {
                match arg.view() {
                    ValueView::Array(items, ..) => {
                        let seps: Vec<Vec<u8>> = items
                            .iter()
                            .map(|v: &Value| v.to_string_value().into_bytes())
                            .collect();
                        state.line_separators = seps;
                    }
                    _ => {
                        let s = arg.to_string_value();
                        state.line_separators = vec![s.clone().into_bytes()];
                    }
                }
                Ok(())
            })?;
            return Ok(arg.clone());
        }
        let got = self.with_handle_mut_opt(target_val, |state| {
            if state.line_separators.len() == 1 {
                Ok(Value::str(
                    String::from_utf8_lossy(&state.line_separators[0]).to_string(),
                ))
            } else {
                let items: Vec<Value> = state
                    .line_separators
                    .iter()
                    .map(|s| Value::str(String::from_utf8_lossy(s).to_string()))
                    .collect();
                Ok(Value::real_array(items))
            }
        })?;
        match got {
            Some(v) => Ok(v),
            None => {
                // Default nl-in for unopened handles
                let items: Vec<Value> = self
                    .default_line_separators()
                    .iter()
                    .map(|s| Value::str(String::from_utf8_lossy(s).to_string()))
                    .collect();
                if items.len() == 1 {
                    Ok(items.into_iter().next().unwrap())
                } else {
                    Ok(Value::real_array(items))
                }
            }
        }
    }

    // Cost: TODO
    pub(crate) fn io_handle_chomp(
        &mut self,
        target_val: &Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let set = args.first().map(|a| a.truthy());
        let chomp =
            self.with_handle_mut(target_val, |state| Ok(state.chomp_setting(set)))?;
        Ok(Value::truth(chomp))
    }

    // Cost: TODO
    pub(crate) fn io_handle_close(
        &mut self,
        target_val: &Value,
        _args: &[Value],
    ) -> Result<Value, RuntimeError> {
        Ok(Value::truth(self.close_handle_value(target_val)?))
    }

    // Cost: TODO
    pub(crate) fn io_handle_flush(
        &mut self,
        target_val: &Value,
        _args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let flushed =
            self.with_handle_mut_opt(target_val, |state| state.flush_for_method())?;
        if flushed.is_some() {
            Ok(Value::TRUE)
        } else {
            let mut ex_attrs = HashMap::new();
            ex_attrs.insert(
                "message".to_string(),
                Value::str_from("Failed to flush handle"),
            );
            let ex = Value::make_instance(Symbol::intern("X::IO::Flush"), ex_attrs);
            let mut failure_attrs = HashMap::new();
            failure_attrs.insert("exception".to_string(), ex);
            Ok(Value::make_instance(
                Symbol::intern("Failure"),
                failure_attrs,
            ))
        }
    }

    // Cost: TODO
    pub(crate) fn io_handle_out_buffer(
        &mut self,
        target_val: &Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let set = args.first().map(Self::parse_out_buffer_size);
        let size =
            self.with_handle_mut(target_val, |state| state.out_buffer_setting(set))?;
        Ok(Value::int(size as i64))
    }

    // Cost: TODO
    pub(crate) fn io_handle_seek(
        &mut self,
        target_val: &Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let pos = args
            .first()
            .and_then(|v| match v.view() {
                ValueView::Int(i) => Some(i),
                _ => None,
            })
            .unwrap_or(0);
        // Second argument: seek mode
        let mode_str = args
            .get(1)
            .map(|v| v.to_string_value())
            .unwrap_or_else(|| "SeekFromBeginning".to_string());
        let seek_mode = match mode_str.as_str() {
            "SeekFromBeginning" => 0,
            "SeekFromCurrent" => 1,
            "SeekFromEnd" => 2,
            _ => 0,
        };
        let offset = self.seek_handle_value(target_val, pos, seek_mode)?;
        Ok(Value::int(offset))
    }

    // Cost: TODO
    pub(crate) fn io_handle_tell(
        &mut self,
        target_val: &Value,
        _args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let position = self.tell_handle_value(target_val)?;
        Ok(Value::int(position))
    }

    // Cost: TODO
    pub(crate) fn io_handle_lock(
        &mut self,
        target_val: &Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let shared = args.iter().any(
            |a| matches!(a.view(), ValueView::Pair(k, v) if k == "shared" && v.truthy()),
        );
        let non_blocking = args
            .iter()
            .any(|a| matches!(a.view(), ValueView::Pair(k, v) if k == "non-blocking" && v.truthy()));
        self.lock_handle_value(target_val, shared, non_blocking)
    }

    // Cost: TODO
    pub(crate) fn io_handle_unlock(
        &mut self,
        target_val: &Value,
        _args: &[Value],
    ) -> Result<Value, RuntimeError> {
        self.unlock_handle_value(target_val)
    }

    // Cost: TODO
    pub(crate) fn io_handle_eof(
        &mut self,
        target_val: &Value,
        _args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let at_end = self.handle_eof_value(target_val)?;
        Ok(Value::truth(at_end))
    }

    // Cost: TODO
    pub(crate) fn io_handle_t(
        &mut self,
        target_val: &Value,
        _args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let is_tty = self.with_handle_mut(target_val, |state| Ok(state.is_tty()))?;
        Ok(Value::truth(is_tty))
    }

    // Cost: TODO
    pub(crate) fn io_handle_encoding(
        &mut self,
        target_val: &Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if let Some(arg) = args.first() {
            // Nil means switch to binary mode (no encoding)
            if arg.is_nil() {
                self.set_handle_encoding(target_val, Some("bin".to_string()))?;
                return Ok(Value::NIL);
            }
            let encoding = arg.to_string_value();
            if encoding == "bin" {
                self.set_handle_encoding(target_val, Some("bin".to_string()))?;
                return Ok(Value::NIL);
            }
            self.set_handle_encoding(target_val, Some(encoding.clone()))?;
            return Ok(Value::str(encoding));
        }
        let current = self.set_handle_encoding(target_val, None)?;
        if current == "bin" {
            return Ok(Value::NIL);
        }
        Ok(Value::str(current))
    }

    // Cost: TODO
    pub(crate) fn io_handle_opened(
        &mut self,
        target_val: &Value,
        _args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let opened = self.with_handle_mut(target_val, |state| Ok(state.is_opened()))?;
        Ok(Value::truth(opened))
    }

    // Cost: TODO
    pub(crate) fn io_handle_native_descriptor(
        &mut self,
        target_val: &Value,
        _args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let fd = self.with_handle_mut(target_val, |state| state.native_descriptor())?;
        Ok(Value::int(fd))
    }

}
