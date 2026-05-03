use super::*;
use crate::value::RuntimeError;

/// Returns true if the value may have a custom `.gist`/`.Str` method that
/// requires interpreter method dispatch.  For all other (primitive) types
/// we can use the fast `gist_value()` / `to_string_value()` paths directly.
fn needs_method_dispatch(v: &Value) -> bool {
    matches!(
        v,
        Value::Instance { .. }
            | Value::CustomType { .. }
            | Value::CustomTypeInstance { .. }
            | Value::Mixin(..)
            | Value::Proxy { .. }
            | Value::Junction { .. }
    )
}

/// Check if a value is a Rat/FatRat/BigRat with zero denominator and throw
/// X::Numeric::DivideByZero if so (Raku defers the error until the value is used).
fn check_rat_divide_by_zero(v: &Value) -> Result<(), RuntimeError> {
    match v {
        Value::Rat(n, d) if *d == 0 => Err(RuntimeError::numeric_divide_by_zero_with(Some(
            Value::Int(*n),
        ))),
        Value::FatRat(n, d) if *d == 0 => Err(RuntimeError::numeric_divide_by_zero_with(Some(
            Value::Int(*n),
        ))),
        Value::BigRat(n, d) if d.is_zero() => Err(RuntimeError::numeric_divide_by_zero_with(Some(
            Value::from_bigint(n.clone()),
        ))),
        _ => Ok(()),
    }
}

impl VM {
    pub(super) fn exec_make_array_op(&mut self, n: u32, is_real_array: bool) {
        let n = n as usize;
        let start = self.stack.len() - n;
        let raw: Vec<Value> = self.stack.drain(start..).collect();
        let mut elems = Vec::with_capacity(raw.len());
        for val in raw {
            // Force lazy IO lines into eager arrays
            let val = if matches!(&val, Value::LazyIoLines { .. }) {
                match self.force_if_lazy_io_lines(val) {
                    Ok(v) => v,
                    Err(_) => continue,
                }
            } else {
                val
            };
            match val {
                Value::Slip(items) => elems.extend(items.iter().cloned()),
                Value::Array(items, kind) if kind.is_itemized() => {
                    elems.push(Value::Array(items, kind))
                }
                // Scalar-wrapped values (.item / $()) are never flattened.
                Value::Scalar(inner) => elems.push(Value::Scalar(inner)),
                // In bracket-array literals (`[...]`), a single element is in
                // list context and should flatten one level (e.g. `[2..6]`,
                // `[@a]`, `[(1,2,3)]`), while multi-element forms keep each
                // element itemized (e.g. `[(1,2),(3,4)]`).
                other if is_real_array && n == 1 => elems.extend(runtime::value_to_list(&other)),
                other => elems.push(other),
            }
        }
        if is_real_array {
            self.stack.push(Value::real_array(elems));
        } else {
            self.stack.push(Value::array(elems));
        }
    }

    /// Like `exec_make_array_op` with `is_real_array=true` but never flattens
    /// single elements. Used for bracket arrays with trailing comma (`[x,]`).
    pub(super) fn exec_make_array_no_flatten_op(&mut self, n: u32) {
        let n = n as usize;
        let start = self.stack.len() - n;
        let raw: Vec<Value> = self.stack.drain(start..).collect();
        let mut elems = Vec::with_capacity(raw.len());
        for val in raw {
            match val {
                Value::Slip(items) => elems.extend(items.iter().cloned()),
                other => elems.push(other),
            }
        }
        self.stack.push(Value::real_array(elems));
    }

    pub(super) fn exec_make_hash_op(&mut self, n: u32) {
        let n = n as usize;
        let start = self.stack.len() - n * 2;
        let items: Vec<Value> = self.stack.drain(start..).collect();
        let mut map = HashMap::new();
        for pair in items.chunks(2) {
            let key = Value::hash_key_encode(&pair[0]);
            let val = pair[1].clone();
            map.insert(key, val);
        }
        self.stack.push(Value::hash(map));
    }

    /// Build a Hash from N Pair values on the stack (from `%(k=>v, ...)` syntax).
    pub(super) fn exec_make_hash_from_pairs_op(&mut self, n: u32) {
        let n = n as usize;
        let start = self.stack.len() - n;
        let items: Vec<Value> = self.stack.drain(start..).collect();
        let mut map = HashMap::new();
        for item in items {
            match item {
                Value::Pair(k, v) => {
                    map.insert(k, *v);
                }
                Value::ValuePair(k, v) => {
                    map.insert(k.to_string_value(), *v);
                }
                _ => {
                    // Non-pair values: use stringified value as key mapped to True
                    map.insert(item.to_string_value(), Value::Bool(true));
                }
            }
        }
        self.stack.push(Value::hash(map));
    }

    pub(super) fn exec_make_capture_op(&mut self, n: u32) {
        let n = n as usize;
        let start = self.stack.len() - n;
        let raw: Vec<Value> = self.stack.drain(start..).collect();
        let mut positional = Vec::new();
        let mut named = HashMap::new();
        for val in raw {
            match val {
                Value::Pair(k, v) => {
                    named.insert(k, *v);
                }
                Value::Capture {
                    positional: p,
                    named: n,
                } => {
                    // Flatten inner capture (from |capture slip)
                    positional.extend(p);
                    named.extend(n);
                }
                Value::Slip(items) => {
                    for item in items.iter() {
                        match item {
                            Value::Pair(k, v) => {
                                named.insert(k.clone(), *v.clone());
                            }
                            other => positional.push(other.clone()),
                        }
                    }
                }
                other => positional.push(other),
            }
        }
        self.stack.push(Value::Capture { positional, named });
    }

    pub(super) fn exec_say_op(&mut self, n: u32) -> Result<(), RuntimeError> {
        let n = n as usize;
        let start = self.stack.len() - n;
        let values: Vec<Value> = self.stack.drain(start..).collect();
        let mut parts = Vec::new();
        for v in &values {
            let v = self.interpreter.auto_fetch_proxy(v)?;
            check_rat_divide_by_zero(&v)?;
            // Resolve bound-element sentinels inside arrays before gist
            let v = self.resolve_bound_array_elements(v);
            if needs_method_dispatch(&v) {
                parts.push(self.interpreter.render_gist_value(&v));
            } else {
                parts.push(runtime::gist_value(&v));
            }
        }
        let line = parts.join("");
        self.interpreter
            .write_to_named_handle("$*OUT", &line, true)?;
        Ok(())
    }

    pub(super) fn exec_note_op(&mut self, n: u32) -> Result<(), RuntimeError> {
        let n = n as usize;
        let content = if n == 0 {
            "Noted".to_string()
        } else {
            let start = self.stack.len() - n;
            let values: Vec<Value> = self.stack.drain(start..).collect();
            let mut parts = Vec::new();
            for v in &values {
                if needs_method_dispatch(v) {
                    parts.push(self.interpreter.render_gist_value(v));
                } else {
                    parts.push(runtime::gist_value(v));
                }
            }
            parts.join("")
        };
        self.interpreter
            .write_to_named_handle("$*ERR", &content, true)?;
        Ok(())
    }

    pub(super) fn exec_put_op(&mut self, n: u32) -> Result<(), RuntimeError> {
        let n = n as usize;
        let start = self.stack.len() - n;
        let values: Vec<Value> = self.stack.drain(start..).collect();
        // put threads through Junctions: each eigenstate gets put individually
        let mut lines = Vec::new();
        for v in &values {
            let v = self.interpreter.auto_fetch_proxy(v)?;
            check_rat_divide_by_zero(&v)?;
            self.collect_put_lines(&v, &mut lines)?;
        }
        for line in &lines {
            self.interpreter
                .write_to_named_handle("$*OUT", line, true)?;
        }
        Ok(())
    }

    pub(super) fn exec_print_op(&mut self, n: u32) -> Result<(), RuntimeError> {
        let n = n as usize;
        let start = self.stack.len() - n;
        let values: Vec<Value> = self.stack.drain(start..).collect();
        let mut content = String::new();
        for v in &values {
            check_rat_divide_by_zero(v)?;
            // For Junctions, thread: call .Str on each element recursively
            self.collect_str_threaded(v, &mut content)?;
        }
        self.interpreter
            .write_to_named_handle("$*OUT", &content, false)?;
        Ok(())
    }

    /// Recursively collect put lines from a value, threading through Junctions.
    fn collect_put_lines(
        &mut self,
        v: &Value,
        lines: &mut Vec<String>,
    ) -> Result<(), RuntimeError> {
        match v {
            Value::Junction { values, .. } => {
                for elem in values.iter() {
                    self.collect_put_lines(elem, lines)?;
                }
            }
            _ if needs_method_dispatch(v) => {
                lines.push(self.interpreter.render_str_value(v));
            }
            _ => {
                lines.push(v.to_str_context());
            }
        }
        Ok(())
    }

    /// Recursively collect .Str output from a value, threading through Junctions.
    fn collect_str_threaded(&mut self, v: &Value, out: &mut String) -> Result<(), RuntimeError> {
        match v {
            Value::Junction { values, .. } => {
                for elem in values.iter() {
                    self.collect_str_threaded(elem, out)?;
                }
            }
            _ if needs_method_dispatch(v) => {
                out.push_str(&self.interpreter.render_str_value(v));
            }
            _ => {
                out.push_str(&v.to_str_context());
            }
        }
        Ok(())
    }
}
