//! say/note/put/print output ops and their rendering helpers,
//! split from `vm_data_ops` (§7-8 file split).
use super::*;
use crate::value::RuntimeError;

/// Returns true if the value may have a custom `.gist`/`.Str` method that
/// requires interpreter method dispatch.  For all other (primitive) types
/// we can use the fast `gist_value()` / `to_string_value()` paths directly.
fn needs_method_dispatch(v: &Value) -> bool {
    match v.view() {
        ValueView::Instance { .. }
        | ValueView::CustomType { .. }
        | ValueView::CustomTypeInstance(_)
        | ValueView::Mixin(..)
        | ValueView::Proxy { .. }
        | ValueView::Junction { .. } => true,
        // Type objects may carry a user-defined `method gist`/`method Str`
        // (callable on the type object itself), so route them through method
        // dispatch; `render_gist_value`/`render_str_value` fall back to the
        // default `(TypeName)` rendering when no such method exists.
        ValueView::Package(..) => true,
        // A LazyList (gather/take, infinite sequence, lazy map/grep pipeline)
        // must be rendered via `.gist`/`.Str` method dispatch: an eager gather
        // is forced to its elements, while a genuinely lazy/infinite one
        // renders as raku's placeholder (`(...)` / `...`). The pure
        // `gist_value`/`to_str_context` fast paths would print the bare type
        // name "LazyList" instead.
        ValueView::LazyList(..) => true,
        // Sub/Routine gist is not the same as their stringification: a named
        // routine gists as `&name` while `.Str` remains the bare name. Route
        // these through the native `.gist` dispatch instead of the fast
        // string-value fallback used by the output op.
        ValueView::Sub(..) | ValueView::WeakSub(..) | ValueView::Routine { .. } => true,
        // A collection whose gist embeds an element's gist must be rendered via
        // method dispatch when any element needs it (e.g. an instance/type-object
        // with a custom `method gist`), so the per-element gist is honored.
        ValueView::Array(..)
        | ValueView::Seq(..)
        | ValueView::HyperSeq(..)
        | ValueView::RaceSeq(..)
        | ValueView::Slip(..)
        | ValueView::Hash(..)
        | ValueView::Pair(..)
        | ValueView::ValuePair(..) => {
            // One visited set for the whole walk (see
            // `element_needs_method_dispatch_seen`): the receiver itself is not
            // an element, so start below it at depth 0.
            let mut seen = std::collections::HashSet::new();
            if let Some(id) = match v.view() {
                ValueView::Array(data, _) => Some(crate::gc::Gc::as_ptr(&data) as usize),
                ValueView::Hash(data) => Some(crate::gc::Gc::as_ptr(&data) as usize),
                _ => None,
            } {
                seen.insert(id);
            }
            let mut probe = |e: &Value| element_needs_method_dispatch_seen(e, &mut seen, 0);
            match v.view() {
                ValueView::Array(items, _) => items.iter().any(&mut probe),
                ValueView::Seq(items) | ValueView::HyperSeq(items) | ValueView::RaceSeq(items) => {
                    items.iter().any(&mut probe)
                }
                ValueView::Slip(items) => items.iter().any(&mut probe),
                ValueView::Hash(map) => map.values().any(&mut probe),
                ValueView::Pair(_, val) => probe(val),
                ValueView::ValuePair(k, val) => probe(k) || probe(val),
                _ => false,
            }
        }
        _ => false,
    }
}

/// Whether a *collection element* must be rendered via method dispatch.
///
/// `seen` holds every `Gc`-backed container already walked — not just the
/// ancestors — so a circular structure (`my @c; @c = 42, @c`) terminates and a
/// graph with two cyclic edges is not re-walked once per path reaching it.
/// Without it `say @c` recursed here, in the probe, until the process aborted on
/// a stack overflow. Same discipline as the `.raku` twin,
/// `contains_dispatch_leaf_seen` in `runtime::methods_raku_dispatch`.
fn element_needs_method_dispatch_seen(
    v: &Value,
    seen: &mut std::collections::HashSet<usize>,
    depth: usize,
) -> bool {
    const MAX_DEPTH: usize = 256;
    if depth > MAX_DEPTH {
        return false;
    }
    if matches!(
        v.view(),
        ValueView::Instance { .. }
            | ValueView::CustomType { .. }
            | ValueView::CustomTypeInstance(_)
            | ValueView::Mixin(..)
            | ValueView::Package(..)
            | ValueView::Sub(..)
            | ValueView::WeakSub(..)
            | ValueView::Routine { .. }
    ) {
        return true;
    }
    let id = match v.view() {
        ValueView::Array(data, _) => Some(crate::gc::Gc::as_ptr(&data) as usize),
        ValueView::Hash(data) => Some(crate::gc::Gc::as_ptr(&data) as usize),
        _ => None,
    };
    if let Some(id) = id
        && !seen.insert(id)
    {
        return false;
    }
    let mut probe = |e: &Value| element_needs_method_dispatch_seen(e, seen, depth + 1);
    match v.view() {
        ValueView::Array(items, _) => items.iter().any(&mut probe),
        ValueView::Seq(items) | ValueView::HyperSeq(items) | ValueView::RaceSeq(items) => {
            items.iter().any(&mut probe)
        }
        ValueView::Slip(items) => items.iter().any(&mut probe),
        ValueView::Hash(map) => map.values().any(&mut probe),
        ValueView::Pair(_, val) => probe(val),
        ValueView::ValuePair(k, val) => probe(k) || probe(val),
        _ => false,
    }
}

/// `say`/`put`/`print`/`note` on an *unhandled* Failure throws its wrapped
/// exception: raku explodes a Failure as soon as `.gist`/`.Str` is called on
/// it (`say 1.0000001 ** 10**90000` dies with X::Numeric::Overflow,
/// A01-limits/overflow.t). A handled Failure renders normally.
fn check_unhandled_failure(v: &Value) -> Result<(), RuntimeError> {
    if let ValueView::Instance {
        class_name,
        attributes,
        ..
    } = v.view()
        && class_name == "Failure"
    {
        let handled = attributes
            .as_map()
            .get("handled")
            .map(|h| h.truthy())
            .unwrap_or(false);
        if !handled && let Some(ex) = attributes.as_map().get("exception").cloned() {
            let ex = crate::runtime::Interpreter::as_exception_value(ex);
            let mut err = RuntimeError::new(ex.to_string_value());
            // Fail-site backtrace for the dual-backtrace rendering (see
            // `failure_value_to_error`).
            if let Some(orig) = crate::runtime::Interpreter::exception_backtrace_text(&ex) {
                err.set_failure_original_backtrace(Some(orig));
            }
            err.exception = Some(Box::new(ex));
            return Err(err);
        }
    }
    Ok(())
}

/// Check if a value is a Rat/FatRat/BigRat with zero denominator and throw
/// X::Numeric::DivideByZero if so (Raku defers the error until the value is used).
fn check_rat_divide_by_zero(v: &Value) -> Result<(), RuntimeError> {
    match v.view() {
        ValueView::Rat(n, 0) => Err(RuntimeError::numeric_divide_by_zero_with(Some(Value::int(
            n,
        )))),
        ValueView::FatRat(n, 0) => Err(RuntimeError::numeric_divide_by_zero_with(Some(
            Value::int(n),
        ))),
        ValueView::BigRat(n, d) if d.is_zero() => Err(RuntimeError::numeric_divide_by_zero_with(
            Some(Value::from_bigint(n.clone())),
        )),
        _ => Ok(()),
    }
}

impl Interpreter {
    /// Flatten top-level `Slip` arguments into the surrounding argument list.
    /// A `|(...)` slip passed to a list operator (say/put/print/note) spreads its
    /// elements as individual arguments, exactly like the parenthesized-call path.
    fn flatten_slip_args(values: Vec<Value>) -> Vec<Value> {
        if !values
            .iter()
            .any(|v| matches!(v.view(), ValueView::Slip(_)))
        {
            return values
                .into_iter()
                .filter(|value| !value.is_string_pair_value())
                .collect();
        }
        let mut out = Vec::with_capacity(values.len());
        for v in values {
            match v.view() {
                ValueView::Slip(items) => out.extend(items.iter().cloned()),
                _ => out.push(v),
            }
        }
        out.into_iter()
            .filter(|value| !value.is_string_pair_value())
            .collect()
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
            // ADR-0040 §9.2: `say` renders its argument, so a `Proxy` anywhere
            // inside it FETCHes — not just a top-level one, which is all this
            // used to do (`say (1, $p, 3)` printed `(1 Proxy 3)`).
            let v = loan_env!(self, resolve_proxies_in_value(v))?;
            check_rat_divide_by_zero(&v)?;
            check_unhandled_failure(&v)?;
            // Resolve bound-element sentinels inside arrays before gist
            let v = self.resolve_bound_array_elements(v);
            if needs_method_dispatch(&v) {
                parts.push(loan_env!(self, render_gist_value(&v))?);
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
                // `note` renders exactly as `say` does (ADR-0040 §9.2).
                let v = loan_env!(self, resolve_proxies_in_value(v))?;
                if needs_method_dispatch(&v) {
                    parts.push(loan_env!(self, render_gist_value(&v))?);
                } else {
                    parts.push(runtime::gist_value(&v));
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
        if values.len() == 1 && matches!(values[0].view(), ValueView::Junction { .. }) {
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
            // Deep, for the reason `say` is — see ADR-0040 §9.2.
            let v = loan_env!(self, resolve_proxies_in_value(v))?;
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
            // `print` renders exactly as `put` does (ADR-0040 §9.2).
            let v = loan_env!(self, resolve_proxies_in_value(v))?;
            check_rat_divide_by_zero(&v)?;
            // For Junctions, thread: call .Str on each element recursively
            self.collect_str_threaded(&v, &mut content)?;
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
        match v.view() {
            ValueView::Junction { values, .. } => {
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
        match v.view() {
            ValueView::Nil => {
                // `print Nil` stringifies via `.Str`, which warns ("Use of Nil
                // in string context") and resumes with the empty string. (`say`
                // uses `.gist` and renders "Nil" without a warning.)
                let resumed = self.raise_resumable_warning(
                    "Use of Nil in string context",
                    Value::str(String::new()),
                )?;
                out.push_str(&resumed.to_string_value());
            }
            ValueView::Junction { values, .. } => {
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
