//! `IO::CatHandle` — a virtual handle that reads from a sequence of source
//! handles/paths one after another. Sources may be `IO::Path`, `Str`, or an
//! already-opened `IO::Handle`; each is opened (paths/strings) and read to
//! exhaustion before transparently switching to the next.
//!
//! State lives in the instance attributes (persisted across calls via the
//! shared-attr writeback in the mutable dispatch path):
//! - `sources`  — Array of the original source values
//! - `pos`      — Int, index of the next source to open
//! - `active`   — the currently-open `IO::Handle` value, or `Nil`
//! - `chomp`    — Bool (applied to each handle as it is opened)
//! - `nl-in`    — line separator(s), Str or Array
//! - `encoding` — Str (default `utf8`)
//! - `closed`   — Bool, set by `.close`/`.DESTROY` (reads then yield `Nil`)
//! - `on-switch`— Callable invoked when the active handle changes, or `Nil`
use super::*;
use std::collections::HashMap;

/// Extract an integer from a `Value::Int` (allomorphs/strings yield `None`).
fn val_int(v: &Value) -> Option<i64> {
    match v {
        Value::Int(i) => Some(*i),
        _ => None,
    }
}

impl Interpreter {
    /// Build a fresh `IO::CatHandle` instance from positional source values and
    /// `:chomp` / `:nl-in` / `:encoding` named args (the `.new` constructor).
    pub(crate) fn build_io_cathandle(&mut self, args: &[Value]) -> Value {
        let mut sources: Vec<Value> = Vec::new();
        let mut chomp = Value::Bool(true);
        let mut nl_in = Value::array(vec![
            Value::str("\n".to_string()),
            Value::str("\r\n".to_string()),
        ]);
        let mut encoding = Value::str("utf8".to_string());
        let mut on_switch = Value::Nil;
        for arg in args {
            match arg {
                Value::Pair(k, v) => match k.as_str() {
                    "chomp" => chomp = (**v).clone(),
                    "nl-in" => nl_in = (**v).clone(),
                    "encoding" | "enc" => encoding = (**v).clone(),
                    "on-switch" => on_switch = (**v).clone(),
                    _ => {}
                },
                // A list/array/Seq argument is flattened into the source
                // sequence. A Seq backed by a deferred (map) iterator carries no
                // materialized items, so force it through `.list` first.
                Value::Array(items, kind) if !kind.is_itemized() => {
                    sources.extend(items.iter().cloned())
                }
                Value::Seq(_) | Value::LazyList(_) => {
                    let listed = self
                        .call_method_with_values(arg.clone(), "list", vec![])
                        .unwrap_or(Value::Nil);
                    sources.extend(Self::value_to_list(&listed));
                }
                other => sources.push(other.clone()),
            }
        }
        let mut attrs: HashMap<String, Value> = HashMap::new();
        attrs.insert("sources".to_string(), Value::array(sources));
        attrs.insert("pos".to_string(), Value::Int(0));
        attrs.insert("active".to_string(), Value::Nil);
        attrs.insert("chomp".to_string(), chomp);
        attrs.insert("nl-in".to_string(), nl_in);
        attrs.insert("encoding".to_string(), encoding);
        attrs.insert("closed".to_string(), Value::Bool(false));
        attrs.insert("on-switch".to_string(), on_switch);
        attrs.insert("path".to_string(), Value::Nil);
        Value::make_instance(Symbol::intern("IO::CatHandle"), attrs)
    }

    /// Open one source value into a read handle, applying the cat's `chomp` /
    /// `nl-in` / `encoding`. Returns `None` when the source cannot be opened.
    fn cat_open_source(&mut self, src: &Value, attrs: &HashMap<String, Value>) -> Option<Value> {
        let chomp = attrs.get("chomp").cloned().unwrap_or(Value::Bool(true));
        let nl_in = attrs
            .get("nl-in")
            .cloned()
            .unwrap_or_else(|| Value::str("\n".to_string()));
        let enc = attrs
            .get("encoding")
            .map(|v| v.to_string_value())
            .unwrap_or_else(|| "utf8".to_string());
        match src {
            Value::Instance { class_name, .. } if class_name == "IO::Handle" => {
                // Already a handle: ensure it is opened, then push the cat's
                // chomp / nl-in / encoding onto it.
                let handle = src.clone();
                let opened = self
                    .native_io_handle_method(&handle, "opened", vec![])
                    .map(|v| v.truthy())
                    .unwrap_or(false);
                let handle = if opened {
                    handle
                } else {
                    match self.native_io_handle_method(
                        &handle,
                        "open",
                        vec![Value::Pair("r".to_string(), Box::new(Value::Bool(true)))],
                    ) {
                        Ok(v @ Value::Instance { .. }) => v,
                        _ => return None,
                    }
                };
                let _ = self.native_io_handle_method(&handle, "chomp", vec![chomp]);
                let _ = self.native_io_handle_method(&handle, "nl-in", vec![nl_in]);
                if enc != "utf8" {
                    let _ =
                        self.native_io_handle_method(&handle, "encoding", vec![Value::str(enc)]);
                }
                Some(handle)
            }
            _ => {
                // IO::Path or Str source: open a fresh read handle.
                let path_str = match src {
                    Value::Instance {
                        class_name,
                        attributes,
                        ..
                    } if class_name == "IO::Path" => attributes
                        .as_map()
                        .get("path")
                        .map(|p| p.to_string_value())
                        .unwrap_or_default(),
                    _ => src.to_string_value(),
                };
                let io_path = self.make_io_path_instance(&path_str);
                let mut h_attrs: HashMap<String, Value> = HashMap::new();
                h_attrs.insert("path".to_string(), io_path);
                let open_args = vec![
                    Value::Pair("r".to_string(), Box::new(Value::Bool(true))),
                    Value::Pair("chomp".to_string(), Box::new(chomp)),
                    Value::Pair("nl-in".to_string(), Box::new(nl_in)),
                    Value::Pair("enc".to_string(), Box::new(Value::str(enc))),
                ];
                match self.native_io_handle(&h_attrs, "open", open_args) {
                    Ok(v) if matches!(&v, Value::Instance { class_name, .. } if class_name == "IO::Handle") => {
                        Some(v)
                    }
                    _ => None,
                }
            }
        }
    }

    /// Invoke a single `IO::Handle` native method on a handle value.
    fn native_io_handle_method(
        &mut self,
        handle: &Value,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        if let Value::Instance { attributes, .. } = handle {
            self.native_io_handle(&attributes.as_map(), method, args)
        } else {
            Ok(Value::Nil)
        }
    }

    /// Close the active handle (if any) and open the next source. Returns the new
    /// active handle, or `Nil` when the sources are exhausted.
    fn cat_next_handle(&mut self, attrs: &mut HashMap<String, Value>) -> Value {
        if let Some(active @ Value::Instance { class_name, .. }) =
            attrs.get("active").cloned().as_ref()
            && class_name == "IO::Handle"
        {
            let _ = self.close_handle_value(active);
        }
        let on_switch = attrs.get("on-switch").cloned().unwrap_or(Value::Nil);
        loop {
            let pos = attrs.get("pos").and_then(val_int).unwrap_or(0) as usize;
            let sources: Vec<Value> = match attrs.get("sources") {
                Some(Value::Array(items, ..)) => items.to_vec(),
                _ => Vec::new(),
            };
            if pos >= sources.len() {
                attrs.insert("active".to_string(), Value::Nil);
                attrs.insert("path".to_string(), Value::Nil);
                return Value::Nil;
            }
            attrs.insert("pos".to_string(), Value::Int((pos + 1) as i64));
            if let Some(handle) = self.cat_open_source(&sources[pos], attrs) {
                let path_val = self
                    .native_io_handle_method(&handle, "path", vec![])
                    .unwrap_or(Value::Nil);
                attrs.insert("active".to_string(), handle.clone());
                attrs.insert("path".to_string(), path_val);
                if matches!(
                    on_switch,
                    Value::Sub(_) | Value::Routine { .. } | Value::WeakSub(_)
                ) {
                    let _ = self
                        .eval_call_on_value(on_switch.clone(), vec![handle.clone(), Value::Nil]);
                }
                return handle;
            }
        }
    }

    /// Ensure there is an active handle, opening the first/next source if needed.
    /// Returns `Nil` when nothing more can be read.
    fn cat_ensure_active(&mut self, attrs: &mut HashMap<String, Value>) -> Value {
        if let Some(active @ Value::Instance { class_name, .. }) =
            attrs.get("active").cloned().as_ref()
            && class_name == "IO::Handle"
        {
            return active.clone();
        }
        // Don't advance the cursor past sources here unless nothing has opened yet.
        let pos = attrs.get("pos").and_then(val_int).unwrap_or(0);
        if pos == 0 {
            return self.cat_next_handle(attrs);
        }
        Value::Nil
    }

    /// Read the next line across all handles, switching when one is exhausted.
    fn cat_get(&mut self, attrs: &mut HashMap<String, Value>) -> Result<Value, RuntimeError> {
        loop {
            let active = self.cat_ensure_active(attrs);
            if matches!(active, Value::Nil) {
                return Ok(Value::Nil);
            }
            let line = self.native_io_handle_method(&active, "get", vec![])?;
            if matches!(line, Value::Nil) {
                let next = self.cat_next_handle(attrs);
                if matches!(next, Value::Nil) {
                    return Ok(Value::Nil);
                }
                continue;
            }
            return Ok(line);
        }
    }

    /// Read one character across all handles.
    fn cat_getc(&mut self, attrs: &mut HashMap<String, Value>) -> Result<Value, RuntimeError> {
        loop {
            let active = self.cat_ensure_active(attrs);
            if matches!(active, Value::Nil) {
                return Ok(Value::Nil);
            }
            let c = self.native_io_handle_method(&active, "getc", vec![])?;
            if matches!(c, Value::Nil) {
                let next = self.cat_next_handle(attrs);
                if matches!(next, Value::Nil) {
                    return Ok(Value::Nil);
                }
                continue;
            }
            return Ok(c);
        }
    }

    /// Slurp the remaining content of all handles into one string.
    fn cat_slurp(&mut self, attrs: &mut HashMap<String, Value>) -> Result<Value, RuntimeError> {
        if attrs.get("closed").is_some_and(|v| v.truthy()) {
            return Ok(Value::Nil);
        }
        let mut out = String::new();
        loop {
            let active = self.cat_ensure_active(attrs);
            if matches!(active, Value::Nil) {
                break;
            }
            let s = self.native_io_handle_method(&active, "slurp", vec![])?;
            out.push_str(&s.to_string_value());
            if matches!(self.cat_next_handle(attrs), Value::Nil) {
                break;
            }
        }
        Ok(Value::str(out))
    }

    /// Collect all remaining lines into a Seq.
    fn cat_lines(&mut self, attrs: &mut HashMap<String, Value>) -> Result<Value, RuntimeError> {
        let mut lines = Vec::new();
        loop {
            let line = self.cat_get(attrs)?;
            if matches!(line, Value::Nil) {
                break;
            }
            lines.push(line);
        }
        Ok(Value::Seq(std::sync::Arc::new(lines)))
    }

    /// Mutable dispatch for `IO::CatHandle` methods. All reads advance internal
    /// state, so every method goes through here and the caller writes the updated
    /// attributes back into the receiver's shared cell.
    pub(crate) fn native_io_cathandle_mut(
        &mut self,
        attributes: HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Result<(Value, HashMap<String, Value>), RuntimeError> {
        let mut attrs = attributes;
        let result = match method {
            "get" => self.cat_get(&mut attrs)?,
            "getc" => self.cat_getc(&mut attrs)?,
            "lines" => self.cat_lines(&mut attrs)?,
            "slurp" => self.cat_slurp(&mut attrs)?,
            "words" => {
                let text = match self.cat_slurp(&mut attrs)? {
                    Value::Nil => return Ok((Value::Nil, attrs)),
                    v => v.to_string_value(),
                };
                let words: Vec<Value> = text
                    .split_whitespace()
                    .map(|w| Value::str(w.to_string()))
                    .collect();
                Value::Seq(std::sync::Arc::new(words))
            }
            "comb" => {
                let text = match self.cat_slurp(&mut attrs)? {
                    Value::Nil => return Ok((Value::Nil, attrs)),
                    v => v.to_string_value(),
                };
                let str_val = Value::str(text);
                self.call_method_with_values(str_val, "comb", args)?
            }
            "split" => {
                let text = match self.cat_slurp(&mut attrs)? {
                    Value::Nil => return Ok((Value::Nil, attrs)),
                    v => v.to_string_value(),
                };
                let str_val = Value::str(text);
                self.call_method_with_values(str_val, "split", args)?
            }
            "readchars" => {
                // Read up to N chars across handles.
                let count = args.first().and_then(val_int).map(|n| n as usize);
                let mut out = String::new();
                loop {
                    let active = self.cat_ensure_active(&mut attrs);
                    if matches!(active, Value::Nil) {
                        break;
                    }
                    let want = count.map(|c| c.saturating_sub(out.chars().count()));
                    if want == Some(0) {
                        break;
                    }
                    let chunk_args = match want {
                        Some(n) => vec![Value::Int(n as i64)],
                        None => vec![],
                    };
                    let chunk = self
                        .native_io_handle_method(&active, "readchars", chunk_args)?
                        .to_string_value();
                    if chunk.is_empty() {
                        if matches!(self.cat_next_handle(&mut attrs), Value::Nil) {
                            break;
                        }
                        continue;
                    }
                    out.push_str(&chunk);
                    if let Some(c) = count
                        && out.chars().count() >= c
                    {
                        break;
                    }
                }
                Value::str(out)
            }
            "read" => {
                // Read up to N bytes across handles, switching when one is
                // exhausted but only while more bytes are still wanted.
                let want = args.first().and_then(val_int).unwrap_or(0).max(0) as usize;
                let mut bytes: Vec<u8> = Vec::new();
                while bytes.len() < want {
                    let active = self.cat_ensure_active(&mut attrs);
                    if matches!(active, Value::Nil) {
                        break;
                    }
                    let remaining = want - bytes.len();
                    let chunk = self.native_io_handle_method(
                        &active,
                        "read",
                        vec![Value::Int(remaining as i64)],
                    )?;
                    let chunk_bytes: Vec<u8> = Self::buf_as_byte_items(&chunk)
                        .unwrap_or_default()
                        .iter()
                        .filter_map(val_int)
                        .map(|b| b as u8)
                        .collect();
                    if chunk_bytes.is_empty() {
                        if matches!(self.cat_next_handle(&mut attrs), Value::Nil) {
                            break;
                        }
                        continue;
                    }
                    bytes.extend_from_slice(&chunk_bytes);
                }
                let mut battrs: HashMap<String, Value> = HashMap::new();
                battrs.insert(
                    "bytes".to_string(),
                    Value::array(bytes.iter().map(|b| Value::Int(*b as i64)).collect()),
                );
                Value::make_instance(Symbol::intern("Buf"), battrs)
            }
            "next-handle" => self.cat_next_handle(&mut attrs),
            "close" | "DESTROY" => {
                if let Some(active @ Value::Instance { class_name, .. }) =
                    attrs.get("active").cloned().as_ref()
                    && class_name == "IO::Handle"
                {
                    let _ = self.close_handle_value(active);
                }
                // Close any remaining un-opened IO::Handle sources too.
                let handle_sources: Vec<Value> = match attrs.get("sources") {
                    Some(Value::Array(items, ..)) => items
                        .iter()
                        .filter(|v| matches!(v, Value::Instance { class_name, .. } if class_name == "IO::Handle"))
                        .cloned()
                        .collect(),
                    _ => Vec::new(),
                };
                for src in &handle_sources {
                    let _ = self.close_handle_value(src);
                }
                attrs.insert("active".to_string(), Value::Nil);
                attrs.insert("path".to_string(), Value::Nil);
                attrs.insert("closed".to_string(), Value::Bool(true));
                let pos = match attrs.get("sources") {
                    Some(Value::Array(items, ..)) => items.len() as i64,
                    _ => 0,
                };
                attrs.insert("pos".to_string(), Value::Int(pos));
                Value::Bool(true)
            }
            "eof" => {
                // At EOF when nothing more can be read.
                if attrs.get("closed").is_some_and(|v| v.truthy()) {
                    Value::Bool(true)
                } else {
                    let active = self.cat_ensure_active(&mut attrs);
                    if matches!(active, Value::Nil) {
                        Value::Bool(true)
                    } else {
                        let e = self.native_io_handle_method(&active, "eof", vec![])?;
                        if e.truthy() {
                            // Peek the next handle to decide true EOF.
                            let next = self.cat_next_handle(&mut attrs);
                            Value::Bool(matches!(next, Value::Nil))
                        } else {
                            Value::Bool(false)
                        }
                    }
                }
            }
            "opened" => Value::Bool(!attrs.get("closed").is_some_and(|v| v.truthy())),
            "chomp" => {
                if let Some(arg) = args.into_iter().next() {
                    attrs.insert("chomp".to_string(), arg.clone());
                    arg
                } else {
                    attrs.get("chomp").cloned().unwrap_or(Value::Bool(true))
                }
            }
            "nl-in" => {
                if let Some(arg) = args.into_iter().next() {
                    attrs.insert("nl-in".to_string(), arg.clone());
                    arg
                } else {
                    attrs
                        .get("nl-in")
                        .cloned()
                        .unwrap_or_else(|| Value::str("\n".to_string()))
                }
            }
            "encoding" => {
                if let Some(arg) = args.into_iter().next() {
                    let s = arg.to_string_value();
                    attrs.insert("encoding".to_string(), Value::str(s.clone()));
                    Value::str(s)
                } else {
                    attrs
                        .get("encoding")
                        .cloned()
                        .unwrap_or_else(|| Value::str("utf8".to_string()))
                }
            }
            "on-switch" => {
                if let Some(arg) = args.into_iter().next() {
                    attrs.insert("on-switch".to_string(), arg.clone());
                    arg
                } else {
                    attrs.get("on-switch").cloned().unwrap_or(Value::Nil)
                }
            }
            "path" | "IO" => {
                // Make sure a handle is active so `.path` reflects the current source.
                if matches!(attrs.get("active"), Some(Value::Nil) | None)
                    && attrs.get("pos").and_then(val_int).unwrap_or(0) == 0
                {
                    let _ = self.cat_ensure_active(&mut attrs);
                }
                attrs.get("path").cloned().unwrap_or(Value::Nil)
            }
            "handles" => {
                // The lazy list of handles this cat will read: the active one (if
                // any) plus the remaining unopened sources opened on demand. For a
                // simple, eager view, return active + remaining-opened handles.
                let mut out = Vec::new();
                loop {
                    let active = self.cat_ensure_active(&mut attrs);
                    if matches!(active, Value::Nil) {
                        break;
                    }
                    out.push(active);
                    if matches!(self.cat_next_handle(&mut attrs), Value::Nil) {
                        break;
                    }
                }
                Value::Seq(std::sync::Arc::new(out))
            }
            "gist" => {
                // Open the first handle if nothing has been read yet, then render
                // `IO::CatHandle(opened on <path>.gist)` or `IO::CatHandle(closed)`.
                if matches!(attrs.get("active"), Some(Value::Nil) | None)
                    && attrs.get("pos").and_then(val_int).unwrap_or(0) == 0
                    && !attrs.get("closed").is_some_and(|v| v.truthy())
                {
                    let _ = self.cat_ensure_active(&mut attrs);
                }
                match attrs.get("active").cloned() {
                    Some(active @ Value::Instance { .. }) => {
                        let path = self
                            .native_io_handle_method(&active, "path", vec![])
                            .unwrap_or(Value::Nil);
                        let path_gist = self
                            .call_method_with_values(path, "gist", vec![])
                            .map(|v| v.to_string_value())
                            .unwrap_or_default();
                        Value::str(format!("IO::CatHandle(opened on {})", path_gist))
                    }
                    _ => Value::str("IO::CatHandle(closed)".to_string()),
                }
            }
            "Str" => {
                // The active handle's path as a string (opening the first handle
                // if needed). On a closed/exhausted cat, fall back to the name.
                if matches!(attrs.get("active"), Some(Value::Nil) | None)
                    && attrs.get("pos").and_then(val_int).unwrap_or(0) == 0
                    && !attrs.get("closed").is_some_and(|v| v.truthy())
                {
                    let _ = self.cat_ensure_active(&mut attrs);
                }
                match attrs.get("path").cloned() {
                    Some(p) if !matches!(p, Value::Nil) => Value::str(p.to_string_value()),
                    _ => Value::str("IO::CatHandle".to_string()),
                }
            }
            "Supply" | "native-descriptor" | "seek" | "tell" | "t" => {
                return Err(RuntimeError::new(format!(
                    "No native mutable method '{}' on 'IO::CatHandle'",
                    method
                )));
            }
            _ => {
                return Err(RuntimeError::new(format!(
                    "No native mutable method '{}' on 'IO::CatHandle'",
                    method
                )));
            }
        };
        Ok((result, attrs))
    }
}
