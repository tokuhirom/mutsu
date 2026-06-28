//! say/note/put/print output ops and their rendering helpers,
//! split from `vm_data_ops` (§7-8 file split).
use super::*;
use crate::value::RuntimeError;

/// Returns true if the value may have a custom `.gist`/`.Str` method that
/// requires interpreter method dispatch.  For all other (primitive) types
/// we can use the fast `gist_value()` / `to_string_value()` paths directly.
fn needs_method_dispatch(v: &Value) -> bool {
    match v {
        Value::Instance { .. }
        | Value::CustomType { .. }
        | Value::CustomTypeInstance(_)
        | Value::Mixin(..)
        | Value::Proxy { .. }
        | Value::Junction { .. } => true,
        // Type objects may carry a user-defined `method gist`/`method Str`
        // (callable on the type object itself), so route them through method
        // dispatch; `render_gist_value`/`render_str_value` fall back to the
        // default `(TypeName)` rendering when no such method exists.
        Value::Package(..) => true,
        // A LazyList (gather/take, infinite sequence, lazy map/grep pipeline)
        // must be rendered via `.gist`/`.Str` method dispatch: an eager gather
        // is forced to its elements, while a genuinely lazy/infinite one
        // renders as raku's placeholder (`(...)` / `...`). The pure
        // `gist_value`/`to_str_context` fast paths would print the bare type
        // name "LazyList" instead.
        Value::LazyList(..) => true,
        // A collection whose gist embeds an element's gist must be rendered via
        // method dispatch when any element needs it (e.g. an instance/type-object
        // with a custom `method gist`), so the per-element gist is honored.
        Value::Array(items, _) => items.iter().any(element_needs_method_dispatch),
        Value::Seq(items) | Value::HyperSeq(items) | Value::RaceSeq(items) | Value::Slip(items) => {
            items.iter().any(element_needs_method_dispatch)
        }
        Value::Hash(map) => map.values().any(element_needs_method_dispatch),
        Value::Pair(_, val) => element_needs_method_dispatch(val),
        Value::ValuePair(k, val) => {
            element_needs_method_dispatch(k) || element_needs_method_dispatch(val)
        }
        _ => false,
    }
}

/// Whether a *collection element* must be rendered via method dispatch. Unlike
/// the top-level check, a Mixin element is excluded: a Mixin wrapping a
/// List/Array renders via its inner value, and dispatching `.gist` on it would
/// add a spurious paren layer (regressing e.g. `(@list but Role).gist`).
fn element_needs_method_dispatch(v: &Value) -> bool {
    match v {
        Value::Instance { .. }
        | Value::CustomType { .. }
        | Value::CustomTypeInstance(_)
        | Value::Package(..) => true,
        Value::Array(items, _) => items.iter().any(element_needs_method_dispatch),
        Value::Seq(items) | Value::HyperSeq(items) | Value::RaceSeq(items) | Value::Slip(items) => {
            items.iter().any(element_needs_method_dispatch)
        }
        Value::Hash(map) => map.values().any(element_needs_method_dispatch),
        Value::Pair(_, val) => element_needs_method_dispatch(val),
        Value::ValuePair(k, val) => {
            element_needs_method_dispatch(k) || element_needs_method_dispatch(val)
        }
        _ => false,
    }
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
            Value::from_bigint((**n).clone()),
        ))),
        _ => Ok(()),
    }
}

impl Interpreter {
    /// Flatten top-level `Slip` arguments into the surrounding argument list.
    /// A `|(...)` slip passed to a list operator (say/put/print/note) spreads its
    /// elements as individual arguments, exactly like the parenthesized-call path.
    fn flatten_slip_args(values: Vec<Value>) -> Vec<Value> {
        if !values.iter().any(|v| matches!(v, Value::Slip(_))) {
            return values;
        }
        let mut out = Vec::with_capacity(values.len());
        for v in values {
            match v {
                Value::Slip(items) => out.extend(items.iter().cloned()),
                other => out.push(other),
            }
        }
        out
    }

    pub(super) fn exec_say_op(&mut self, n: u32) -> Result<(), RuntimeError> {
        let n = n as usize;
        let start = self.stack.len() - n;
        let values: Vec<Value> = Self::flatten_slip_args(self.stack.drain(start..).collect());
        // Slice F: a user `.gist`/`.Str` closure run below can mutate a
        // captured-outer caller lexical (`say $x but role { method gist {$seen=1} }`).
        // `say` is a dedicated op (no `code` param), so capture the caller frame's
        // code before any dispatch clobbers `current_code` and reconcile after.
        let caller_code = self.current_code;
        let mut parts = Vec::new();
        for v in &values {
            let v = loan_env!(self, auto_fetch_proxy(v))?;
            check_rat_divide_by_zero(&v)?;
            // Resolve bound-element sentinels inside arrays before gist
            let v = self.resolve_bound_array_elements(v);
            if needs_method_dispatch(&v) {
                parts.push(loan_env!(self, render_gist_value(&v)));
            } else {
                parts.push(runtime::gist_value(&v));
            }
        }
        self.reconcile_caller_after_internal_dispatch(caller_code);
        let line = parts.join("");
        loan_env!(self, write_to_named_handle("$*OUT", &line, true))?;
        Ok(())
    }

    pub(super) fn exec_note_op(&mut self, n: u32) -> Result<(), RuntimeError> {
        let n = n as usize;
        let content = if n == 0 {
            "Noted".to_string()
        } else {
            let start = self.stack.len() - n;
            let values: Vec<Value> = Self::flatten_slip_args(self.stack.drain(start..).collect());
            // Slice F: see exec_say_op — reconcile after a user `.gist` closure.
            let caller_code = self.current_code;
            let mut parts = Vec::new();
            for v in &values {
                if needs_method_dispatch(v) {
                    parts.push(loan_env!(self, render_gist_value(v)));
                } else {
                    parts.push(runtime::gist_value(v));
                }
            }
            self.reconcile_caller_after_internal_dispatch(caller_code);
            parts.join("")
        };
        loan_env!(self, write_to_named_handle("$*ERR", &content, true))?;
        Ok(())
    }

    pub(super) fn exec_put_op(&mut self, n: u32) -> Result<(), RuntimeError> {
        let n = n as usize;
        let start = self.stack.len() - n;
        let values: Vec<Value> = Self::flatten_slip_args(self.stack.drain(start..).collect());
        // A lone Junction argument autothreads: each eigenstate is put on its
        // own line (`put 1|2` => "1\n2\n").
        if values.len() == 1 && matches!(&values[0], Value::Junction { .. }) {
            let v = loan_env!(self, auto_fetch_proxy(&values[0]))?;
            check_rat_divide_by_zero(&v)?;
            let mut lines = Vec::new();
            self.collect_put_lines(&v, &mut lines)?;
            for line in &lines {
                loan_env!(self, write_to_named_handle("$*OUT", line, true))?;
            }
            return Ok(());
        }
        // Otherwise concatenate every argument's `.Str` into a single line plus a
        // trailing newline (`put 1, 2, 3` => "123\n"), like `print` with a newline.
        // Slice F: a user `.Str` closure run below can mutate a captured-outer
        // caller lexical; capture the caller frame's code and reconcile after (see
        // exec_say_op).
        let caller_code = self.current_code;
        let mut content = String::new();
        for v in &values {
            let v = loan_env!(self, auto_fetch_proxy(v))?;
            check_rat_divide_by_zero(&v)?;
            if needs_method_dispatch(&v) {
                content.push_str(&loan_env!(self, render_str_value(&v)));
            } else {
                content.push_str(&v.to_str_context());
            }
        }
        self.reconcile_caller_after_internal_dispatch(caller_code);
        loan_env!(self, write_to_named_handle("$*OUT", &content, true))?;
        Ok(())
    }

    pub(super) fn exec_print_op(&mut self, n: u32) -> Result<(), RuntimeError> {
        let n = n as usize;
        let start = self.stack.len() - n;
        let values: Vec<Value> = Self::flatten_slip_args(self.stack.drain(start..).collect());
        // Slice F: see exec_put_op — reconcile after a user `.Str` closure.
        let caller_code = self.current_code;
        let mut content = String::new();
        for v in &values {
            check_rat_divide_by_zero(v)?;
            // For Junctions, thread: call .Str on each element recursively
            self.collect_str_threaded(v, &mut content)?;
        }
        self.reconcile_caller_after_internal_dispatch(caller_code);
        loan_env!(self, write_to_named_handle("$*OUT", &content, false))?;
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
                lines.push(loan_env!(self, render_str_value(v)));
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
                out.push_str(&loan_env!(self, render_str_value(v)));
            }
            _ => {
                out.push_str(&v.to_str_context());
            }
        }
        Ok(())
    }
}
