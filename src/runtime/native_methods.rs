use super::*;

impl Interpreter {
    /// Dispatch a mutable native instance method.
    /// Returns (result_value, updated_attributes).
    pub(super) fn call_native_instance_method_mut(
        &mut self,
        class_name: &str,
        attributes: HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Result<(Value, HashMap<String, Value>), RuntimeError> {
        match class_name {
            "Promise" => self.native_promise_mut(attributes, method, args),
            "Channel" => self.native_channel_mut(attributes, method, args),
            "Supply" => self.native_supply_mut(attributes, method, args),
            "Supplier" => self.native_supplier_mut(attributes, method, args),
            "Proc::Async" => self.native_proc_async_mut(attributes, method, args),
            _ => Err(RuntimeError::new(format!(
                "No native mutable method '{}' on '{}'",
                method, class_name
            ))),
        }
    }

    /// Dispatch an immutable native instance method.
    pub(super) fn call_native_instance_method(
        &mut self,
        class_name: &str,
        attributes: &HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        match class_name {
            "IO::Path" => self.native_io_path(attributes, method, args),
            "IO::Handle" => self.native_io_handle(attributes, method, args),
            "IO::Socket::INET" => self.native_socket_inet(attributes, method, args),
            "IO::Pipe" => self.native_io_pipe(attributes, method),
            "Distro" => self.native_distro(attributes, method),
            "Perl" => Ok(self.native_perl(attributes, method)),
            "Compiler" => Ok(self.native_perl(attributes, method)),
            "Promise" => self.native_promise(attributes, method, args),
            "Channel" => Ok(self.native_channel(attributes, method)),
            "Proc::Async" => Ok(self.native_proc_async(attributes, method)),
            "Supply" => self.native_supply(attributes, method, args),
            "Supplier" => self.native_supplier(attributes, method, args),
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on '{}'",
                method, class_name
            ))),
        }
    }

    // --- Promise mutable ---

    fn native_promise_mut(
        &mut self,
        mut attrs: HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Result<(Value, HashMap<String, Value>), RuntimeError> {
        match method {
            "keep" => {
                let value = args.first().cloned().unwrap_or(Value::Nil);
                attrs.insert("result".to_string(), value);
                attrs.insert("status".to_string(), Value::Str("Kept".to_string()));
                Ok((Value::Nil, attrs))
            }
            _ => Err(RuntimeError::new(format!(
                "No native mutable method '{}' on Promise",
                method
            ))),
        }
    }

    // --- Channel mutable ---

    fn native_channel_mut(
        &mut self,
        mut attrs: HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Result<(Value, HashMap<String, Value>), RuntimeError> {
        match method {
            "send" => {
                let value = args.first().cloned().unwrap_or(Value::Nil);
                match attrs.get_mut("queue") {
                    Some(Value::Array(items)) => items.push(value),
                    _ => {
                        attrs.insert("queue".to_string(), Value::Array(vec![value]));
                    }
                }
                Ok((Value::Nil, attrs))
            }
            "receive" => {
                let mut value = Value::Nil;
                if let Some(Value::Array(items)) = attrs.get_mut("queue")
                    && !items.is_empty()
                {
                    value = items.remove(0);
                }
                Ok((value, attrs))
            }
            "close" => {
                attrs.insert("closed".to_string(), Value::Bool(true));
                Ok((Value::Nil, attrs))
            }
            _ => Err(RuntimeError::new(format!(
                "No native mutable method '{}' on Channel",
                method
            ))),
        }
    }

    // --- Supply immutable ---

    fn native_supply(
        &mut self,
        attributes: &HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        match method {
            "repeated" => {
                let as_fn = Self::named_value(&args, "as");
                let with_fn = Self::named_value(&args, "with");
                let values = match attributes.get("values") {
                    Some(Value::Array(items)) => items.clone(),
                    _ => Vec::new(),
                };
                let mut seen_keys: Vec<Value> = Vec::new();
                let mut result = Vec::new();
                for val in &values {
                    let key = if let Some(ref f) = as_fn {
                        self.call_sub_value(f.clone(), vec![val.clone()], true)?
                    } else {
                        val.clone()
                    };
                    let found = seen_keys.iter().any(|s| {
                        if let Some(ref f) = with_fn {
                            self.call_sub_value(f.clone(), vec![s.clone(), key.clone()], true)
                                .map(|v| v.truthy())
                                .unwrap_or(false)
                        } else {
                            s == &key
                        }
                    });
                    if found {
                        result.push(val.clone());
                    } else {
                        seen_keys.push(key);
                    }
                }
                let mut new_attrs = HashMap::new();
                new_attrs.insert("values".to_string(), Value::Array(result));
                new_attrs.insert("taps".to_string(), Value::Array(Vec::new()));
                new_attrs.insert("live".to_string(), Value::Bool(false));
                Ok(Value::make_instance("Supply".to_string(), new_attrs))
            }
            "tap" => {
                // Immutable tap: emit all values and return a Tap instance
                let tap_cb = args.first().cloned().unwrap_or(Value::Nil);
                let done_cb = Self::named_value(&args, "done");

                // For on-demand supplies, execute the callback to produce values
                let values = if let Some(on_demand_cb) = attributes.get("on_demand_callback") {
                    let emitter = Value::make_instance("Supplier".to_string(), {
                        let mut a = HashMap::new();
                        a.insert("emitted".to_string(), Value::Array(Vec::new()));
                        a.insert("done".to_string(), Value::Bool(false));
                        a
                    });
                    // Use supply_emit_buffer to collect emitted values
                    self.supply_emit_buffer.push(Vec::new());
                    let _ = self.call_sub_value(on_demand_cb.clone(), vec![emitter], false);
                    self.supply_emit_buffer.pop().unwrap_or_default()
                } else if let Some(Value::Array(v)) = attributes.get("values") {
                    v.clone()
                } else {
                    Vec::new()
                };

                // Call do_callbacks and tap callback for each value
                let do_cbs = attributes.get("do_callbacks").and_then(|v| {
                    if let Value::Array(a) = v {
                        Some(a.clone())
                    } else {
                        None
                    }
                });
                for v in &values {
                    if let Some(ref cbs) = do_cbs {
                        for cb in cbs {
                            let _ = self.call_sub_value(cb.clone(), vec![v.clone()], true);
                        }
                    }
                    let _ = self.call_sub_value(tap_cb.clone(), vec![v.clone()], true);
                }

                if let Some(done_fn) = done_cb {
                    let _ = self.call_sub_value(done_fn, vec![], true);
                }
                Ok(Value::make_instance("Tap".to_string(), HashMap::new()))
            }
            "do" => {
                // Supply.do($callback) — create a new Supply that calls $callback
                // as a side-effect for each value, passing values through
                let callback = args.first().cloned().unwrap_or(Value::Nil);
                let values = attributes
                    .get("values")
                    .cloned()
                    .unwrap_or(Value::Array(Vec::new()));
                let live = attributes
                    .get("live")
                    .cloned()
                    .unwrap_or(Value::Bool(false));
                let mut new_attrs = HashMap::new();
                new_attrs.insert("values".to_string(), values);
                new_attrs.insert("taps".to_string(), Value::Array(Vec::new()));
                new_attrs.insert("live".to_string(), live);
                // Accumulate do_callbacks chain
                let mut do_cbs =
                    if let Some(Value::Array(existing)) = attributes.get("do_callbacks") {
                        existing.clone()
                    } else {
                        Vec::new()
                    };
                do_cbs.push(callback);
                new_attrs.insert("do_callbacks".to_string(), Value::Array(do_cbs));
                Ok(Value::make_instance("Supply".to_string(), new_attrs))
            }
            "Supply" | "supply" => {
                // .Supply on a Supply is identity (noop) — return self
                // Preserve the same id for === identity check
                Ok(Value::Instance {
                    class_name: "Supply".to_string(),
                    attributes: attributes.clone(),
                    id: 0, // placeholder — identity is checked via container, not id
                })
            }
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on Supply",
                method
            ))),
        }
    }

    // --- Supplier immutable ---

    fn native_supplier(
        &mut self,
        attributes: &HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        match method {
            "Supply" => {
                // Return a Supply backed by this Supplier
                let mut supply_attrs = HashMap::new();
                supply_attrs.insert("values".to_string(), Value::Array(Vec::new()));
                supply_attrs.insert("taps".to_string(), Value::Array(Vec::new()));
                supply_attrs.insert(
                    "live".to_string(),
                    attributes.get("live").cloned().unwrap_or(Value::Bool(true)),
                );
                Ok(Value::make_instance("Supply".to_string(), supply_attrs))
            }
            "emit" => {
                // Push to supply_emit_buffer (works for on-demand callbacks)
                let value = args.first().cloned().unwrap_or(Value::Nil);
                if let Some(buf) = self.supply_emit_buffer.last_mut() {
                    buf.push(value);
                }
                Ok(Value::Nil)
            }
            "done" => Ok(Value::Nil),
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on Supplier",
                method
            ))),
        }
    }

    // --- Supplier mutable ---

    fn native_supplier_mut(
        &mut self,
        mut attrs: HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Result<(Value, HashMap<String, Value>), RuntimeError> {
        match method {
            "emit" => {
                let value = args.first().cloned().unwrap_or(Value::Nil);
                // Push to supply_emit_buffer if active
                if let Some(buf) = self.supply_emit_buffer.last_mut() {
                    buf.push(value);
                }
                Ok((Value::Nil, attrs))
            }
            "done" => {
                attrs.insert("done".to_string(), Value::Bool(true));
                Ok((Value::Nil, attrs))
            }
            _ => Err(RuntimeError::new(format!(
                "No native mutable method '{}' on Supplier",
                method
            ))),
        }
    }

    // --- Supply mutable ---

    fn native_supply_mut(
        &mut self,
        mut attrs: HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Result<(Value, HashMap<String, Value>), RuntimeError> {
        match method {
            "emit" => {
                let value = args.first().cloned().unwrap_or(Value::Nil);
                if let Some(Value::Array(items)) = attrs.get_mut("values") {
                    items.push(value.clone());
                } else {
                    attrs.insert("values".to_string(), Value::Array(vec![value.clone()]));
                }
                if let Some(Value::Array(taps)) = attrs.get_mut("taps") {
                    for tap in taps.clone() {
                        let _ = self.call_sub_value(tap, vec![value.clone()], true);
                    }
                }
                Ok((Value::Nil, attrs))
            }
            "tap" => {
                let tap_cb = args.first().cloned().unwrap_or(Value::Nil);
                let done_cb = Self::named_value(&args, "done");

                // For on-demand supplies, execute the callback to produce values
                let values = if let Some(on_demand_cb) = attrs.get("on_demand_callback").cloned() {
                    let emitter = Value::make_instance("Supplier".to_string(), {
                        let mut a = HashMap::new();
                        a.insert("emitted".to_string(), Value::Array(Vec::new()));
                        a.insert("done".to_string(), Value::Bool(false));
                        a
                    });
                    self.supply_emit_buffer.push(Vec::new());
                    let _ = self.call_sub_value(on_demand_cb, vec![emitter], false);
                    self.supply_emit_buffer.pop().unwrap_or_default()
                } else {
                    if let Some(Value::Array(items)) = attrs.get_mut("taps") {
                        items.push(tap_cb.clone());
                    } else {
                        attrs.insert("taps".to_string(), Value::Array(vec![tap_cb.clone()]));
                    }
                    if let Some(Value::Array(values)) = attrs.get("values") {
                        values.clone()
                    } else {
                        Vec::new()
                    }
                };

                // Call do_callbacks and tap callback for each value
                let do_cbs = attrs.get("do_callbacks").and_then(|v| {
                    if let Value::Array(a) = v {
                        Some(a.clone())
                    } else {
                        None
                    }
                });
                for v in &values {
                    if let Some(ref cbs) = do_cbs {
                        for cb in cbs {
                            let _ = self.call_sub_value(cb.clone(), vec![v.clone()], true);
                        }
                    }
                    let _ = self.call_sub_value(tap_cb.clone(), vec![v.clone()], true);
                }

                // Call done callback after all values emitted
                if let Some(done_fn) = done_cb {
                    let _ = self.call_sub_value(done_fn, vec![], true);
                }
                let tap_instance = Value::make_instance("Tap".to_string(), HashMap::new());
                Ok((tap_instance, attrs))
            }
            "repeated" => {
                let as_fn = Self::named_value(&args, "as");
                let with_fn = Self::named_value(&args, "with");
                let values = match attrs.get("values") {
                    Some(Value::Array(items)) => items.clone(),
                    _ => Vec::new(),
                };
                let mut seen_keys: Vec<Value> = Vec::new();
                let mut result = Vec::new();
                for val in &values {
                    let key = if let Some(ref f) = as_fn {
                        self.call_sub_value(f.clone(), vec![val.clone()], true)?
                    } else {
                        val.clone()
                    };
                    let found = seen_keys.iter().any(|s| {
                        if let Some(ref f) = with_fn {
                            self.call_sub_value(f.clone(), vec![s.clone(), key.clone()], true)
                                .map(|v| v.truthy())
                                .unwrap_or(false)
                        } else {
                            s == &key
                        }
                    });
                    if found {
                        result.push(val.clone());
                    } else {
                        seen_keys.push(key);
                    }
                }
                let mut new_attrs = HashMap::new();
                new_attrs.insert("values".to_string(), Value::Array(result));
                new_attrs.insert("taps".to_string(), Value::Array(Vec::new()));
                new_attrs.insert("live".to_string(), Value::Bool(false));
                Ok((Value::make_instance("Supply".to_string(), new_attrs), attrs))
            }
            _ => Err(RuntimeError::new(format!(
                "No native mutable method '{}' on Supply",
                method
            ))),
        }
    }

    // --- Proc::Async mutable ---

    fn native_proc_async_mut(
        &mut self,
        mut attrs: HashMap<String, Value>,
        method: &str,
        _args: Vec<Value>,
    ) -> Result<(Value, HashMap<String, Value>), RuntimeError> {
        match method {
            "start" => {
                attrs.insert("started".to_string(), Value::Bool(true));
                let result = self.make_promise_instance("Kept", Value::Int(0));
                Ok((result, attrs))
            }
            _ => Err(RuntimeError::new(format!(
                "No native mutable method '{}' on Proc::Async",
                method
            ))),
        }
    }

    // --- Promise immutable ---

    fn native_promise(
        &mut self,
        attributes: &HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        match method {
            "result" => Ok(attributes.get("result").cloned().unwrap_or(Value::Nil)),
            "status" => Ok(attributes
                .get("status")
                .cloned()
                .unwrap_or(Value::Str("Planned".to_string()))),
            "then" => {
                let block = args.first().cloned().unwrap_or(Value::Nil);
                let status = attributes
                    .get("status")
                    .cloned()
                    .unwrap_or(Value::Str("Planned".to_string()));
                if matches!(status, Value::Str(ref s) if s == "Kept") {
                    let value = attributes.get("result").cloned().unwrap_or(Value::Nil);
                    let result = self.call_sub_value(block, vec![value], false)?;
                    Ok(self.make_promise_instance("Kept", result))
                } else {
                    Ok(self.make_promise_instance("Planned", Value::Nil))
                }
            }
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on Promise",
                method
            ))),
        }
    }

    // --- Channel immutable ---

    fn native_channel(&self, attributes: &HashMap<String, Value>, method: &str) -> Value {
        match method {
            "closed" => attributes
                .get("closed")
                .cloned()
                .unwrap_or(Value::Bool(false)),
            _ => Value::Nil,
        }
    }

    // --- Proc::Async immutable ---

    fn native_proc_async(&self, attributes: &HashMap<String, Value>, method: &str) -> Value {
        match method {
            "command" => attributes
                .get("cmd")
                .cloned()
                .unwrap_or(Value::Array(Vec::new())),
            "started" => attributes
                .get("started")
                .cloned()
                .unwrap_or(Value::Bool(false)),
            "stdout" => attributes.get("stdout").cloned().unwrap_or(Value::Nil),
            "stderr" => attributes.get("stderr").cloned().unwrap_or(Value::Nil),
            _ => Value::Nil,
        }
    }

    // --- Distro ---

    fn native_distro(
        &self,
        attributes: &HashMap<String, Value>,
        method: &str,
    ) -> Result<Value, RuntimeError> {
        match method {
            "name" | "auth" | "desc" | "release" | "path-sep" | "is-win" | "version"
            | "signature" => Ok(attributes.get(method).cloned().unwrap_or(Value::Nil)),
            "gist" => {
                let n = attributes
                    .get("name")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let v = attributes
                    .get("version")
                    .map(|v| {
                        if let Value::Version { parts, .. } = v {
                            Value::version_parts_to_string(parts)
                        } else {
                            v.to_string_value()
                        }
                    })
                    .unwrap_or_default();
                Ok(Value::Str(format!("{} ({})", n, v)))
            }
            "Str" => {
                let n = attributes
                    .get("name")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                Ok(Value::Str(n))
            }
            "raku" | "perl" => {
                let release = attributes
                    .get("release")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let path_sep = attributes
                    .get("path-sep")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let n = attributes
                    .get("name")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let auth = attributes
                    .get("auth")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let ver = attributes
                    .get("version")
                    .map(|v| {
                        if let Value::Version { parts, .. } = v {
                            format!("v{}", Value::version_parts_to_string(parts))
                        } else {
                            v.to_string_value()
                        }
                    })
                    .unwrap_or_default();
                let desc = attributes
                    .get("desc")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                Ok(Value::Str(format!(
                    "Distro.new(release => \"{}\", path-sep => \"{}\", name => \"{}\", auth => \"{}\", version => {}, signature => Blob, desc => \"{}\")",
                    release, path_sep, n, auth, ver, desc
                )))
            }
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on Distro",
                method
            ))),
        }
    }

    // --- Perl ---

    fn native_perl(&self, attributes: &HashMap<String, Value>, method: &str) -> Value {
        match method {
            "compiler" => {
                let mut compiler_attrs = HashMap::new();
                compiler_attrs.insert("name".to_string(), Value::Str("mutsu".to_string()));
                compiler_attrs.insert(
                    "auth".to_string(),
                    Value::Str("github.com/tokuhirom".to_string()),
                );
                compiler_attrs.insert("version".to_string(), Value::Str("v0.1.0".to_string()));
                compiler_attrs.insert("signature".to_string(), Value::Array(vec![Value::Int(0)]));
                compiler_attrs.insert(
                    "desc".to_string(),
                    Value::Str("mutsu Raku interpreter".to_string()),
                );
                compiler_attrs.insert("release".to_string(), Value::Str("0.1.0".to_string()));
                compiler_attrs.insert("codename".to_string(), Value::Str("mutsu".to_string()));
                compiler_attrs.insert("id".to_string(), Value::Str(String::new()));
                Value::make_instance("Compiler".to_string(), compiler_attrs)
            }
            "backend" => Value::Str("mutsu".to_string()),
            "gist" => {
                let name = attributes
                    .get("name")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let version = attributes
                    .get("version")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                Value::Str(format!("{} ({})", name, version))
            }
            "Str" => {
                let name = attributes
                    .get("name")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                Value::Str(name)
            }
            "raku" => {
                let name = attributes
                    .get("name")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                Value::Str(format!("{}.new(...)", name))
            }
            _ => attributes.get(method).cloned().unwrap_or(Value::Nil),
        }
    }

    // --- IO::Socket::INET ---

    fn native_socket_inet(
        &mut self,
        attributes: &HashMap<String, Value>,
        method: &str,
        _args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let handle_id = attributes.get("handle").and_then(|v| {
            if let Value::Int(i) = v {
                Some(*i as usize)
            } else {
                None
            }
        });
        match method {
            "getpeername" => {
                let id =
                    handle_id.ok_or_else(|| RuntimeError::new("IO::Socket::INET has no handle"))?;
                let state = self
                    .handles
                    .get(&id)
                    .ok_or_else(|| RuntimeError::new("Invalid socket handle"))?;
                if state.closed {
                    return Err(RuntimeError::new("Socket is closed"));
                }
                if let Some(ref stream) = state.socket {
                    let addr = stream
                        .peer_addr()
                        .map_err(|e| RuntimeError::new(format!("getpeername failed: {}", e)))?;
                    Ok(Value::Str(addr.to_string()))
                } else {
                    Err(RuntimeError::new("Socket not connected"))
                }
            }
            "close" => {
                let id =
                    handle_id.ok_or_else(|| RuntimeError::new("IO::Socket::INET has no handle"))?;
                let state = self
                    .handles
                    .get_mut(&id)
                    .ok_or_else(|| RuntimeError::new("Invalid socket handle"))?;
                state.closed = true;
                state.socket = None;
                Ok(Value::Bool(true))
            }
            _ => Err(RuntimeError::new(format!(
                "No method '{}' on IO::Socket::INET",
                method
            ))),
        }
    }
}
