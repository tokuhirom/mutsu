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
            let key = pair[0].to_string_value();
            let val = pair[1].clone();
            map.insert(key, val);
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
        let mut content = String::new();
        for v in &values {
            let v = self.interpreter.auto_fetch_proxy(v)?;
            check_rat_divide_by_zero(&v)?;
            if needs_method_dispatch(&v) {
                content.push_str(&self.interpreter.render_str_value(&v));
            } else {
                content.push_str(&v.to_string_value());
            }
        }
        self.interpreter
            .write_to_named_handle("$*OUT", &content, true)?;
        Ok(())
    }

    pub(super) fn exec_print_op(&mut self, n: u32) -> Result<(), RuntimeError> {
        let n = n as usize;
        let start = self.stack.len() - n;
        let values: Vec<Value> = self.stack.drain(start..).collect();
        let mut content = String::new();
        for v in &values {
            check_rat_divide_by_zero(v)?;
            content.push_str(&v.to_string_value());
        }
        self.interpreter
            .write_to_named_handle("$*OUT", &content, false)?;
        Ok(())
    }
}
