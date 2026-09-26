//! say/note/put/print output ops and their rendering helpers,
//! split from `vm_data_ops` (§7-8 file split).
use super::*;
use crate::value::RuntimeError;

/// Returns true if the value may have a custom `.gist`/`.Str` method that
/// requires interpreter method dispatch.  For all other (primitive) types
/// we can use the fast `gist_value()` / `to_string_value()` paths directly.
fn needs_method_dispatch(v: &Value) -> bool {
    match v.view() {
        // A `ContainerRef` is a container, not a type: what it holds decides
        // how the value renders. Rendering asks the pure `gist_value` path
        // otherwise, which prints an `Instance` as the bare `TypeName()`
        // placeholder and never reaches the user's `method gist`/`method Str`
        // (`sub f(\x) is raw { x }; say f($obj)` printed `Thing()`).
        ValueView::ContainerRef(_) | ValueView::ContainerView(_) => true,
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
    // A container is transparent to rendering, exactly as the top-level
    // `needs_method_dispatch` arm says — but an ELEMENT cell must be looked
    // THROUGH rather than answered `true` outright, so an ordinary cell of
    // Ints keeps the pure fast path. Without these two arms every element
    // producer that hands out live cells (`.values`, `.pairs`, `.kv`, `.Seq`,
    // `.sort` — `Value::seq_element_containers` in `vm_element_producers.rs`)
    // reported "no dispatch needed" for a collection of objects, and the
    // whole collection rendered through the pure path: `say @a.values` printed
    // `(F() F())` instead of running the class's own `method gist`, while the
    // same elements gisted correctly through `@a` itself or through an
    // explicit `.gist`. The `.raku` twin named above already looks through
    // both (GH #8134).
    if let ValueView::Scalar(inner) = v.view() {
        return element_needs_method_dispatch_seen(inner, seen, depth + 1);
    }
    if let ValueView::ContainerRef(cell) = v.view() {
        let inner = cell.lock().unwrap().clone();
        return element_needs_method_dispatch_seen(&inner, seen, depth + 1);
    }
    if let ValueView::ContainerView(_) = v.view() {
        let inner = v.deref_container();
        return element_needs_method_dispatch_seen(&inner, seen, depth + 1);
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
        // The one "is this Failure handled?" answer (`.handled`, method-call
        // explosion and sinking all ask it): `.so`/`.Bool`/`.handled = True`
        // record it by instance id, which the `handled` attribute alone misses.
        if !v.is_failure_handled()
            && let Some(ex) = attributes.as_map().get("exception").cloned()
        {
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

/// Which of the four output routines.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum OutputKind {
    Say,
    Put,
    Print,
    Note,
}

impl OutputKind {
    /// The routine a name spells, if it is one of the four.
    pub(crate) fn from_name(name: &str) -> Option<Self> {
        Some(match name {
            "say" => OutputKind::Say,
            "put" => OutputKind::Put,
            "print" => OutputKind::Print,
            "note" => OutputKind::Note,
            _ => return None,
        })
    }

    fn handle(self) -> &'static str {
        if self == OutputKind::Note {
            "$*ERR"
        } else {
            "$*OUT"
        }
    }

    fn appends_newline(self) -> bool {
        self != OutputKind::Print
    }

    fn renders_gist(self) -> bool {
        matches!(self, OutputKind::Say | OutputKind::Note)
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

    /// The four output opcodes: pop `n` arguments and hand them to
    /// [`Interpreter::render_output`].
    // Cost: as `render_output`.
    pub(super) fn exec_output_op(&mut self, kind: OutputKind, n: u32) -> Result<(), RuntimeError> {
        let start = self.stack.len() - n as usize;
        let values: Vec<Value> = self.stack.drain(start..).collect();
        self.render_output(kind, values)
    }

    /// `say`, `put`, `print` and `note`: the one renderer behind every form --
    /// the opcodes, the routine form (`&say(...)`, `my &s = &say; s(...)`) and
    /// anything else that reaches `builtin_print`. They used to be five
    /// bodies applying different subsets of the checks below, so `note 1/0`
    /// and `&put(1/0)` printed `Inf` where every other form died (#9449).
    ///
    /// Every argument, for every kind: Proxies anywhere inside it are FETCHed
    /// (ADR-0040 §9.2), a zero-denominator Rational dies, and an unhandled
    /// Failure throws, because rendering it calls `.gist`/`.Str` on it. `say`
    /// and `note` then render `.gist`; `put` and `print` render `.Str`, which
    /// warns on `Nil` and on a `Regex` and threads a Junction. `put` of a lone
    /// Junction prints one line per eigenstate.
    // Cost: O(t) per argument, t = rendered size: for `say`/`note` the gist head
    // of a list (at most 100 elements per level, see `gist_head`); for
    // `put`/`print` the whole aggregate (`.Str` renders every element).
    pub(crate) fn render_output(
        &mut self,
        kind: OutputKind,
        values: Vec<Value>,
    ) -> Result<(), RuntimeError> {
        let values = Self::flatten_slip_args(values);
        if kind == OutputKind::Note && values.is_empty() {
            return self.write_to_named_handle("$*ERR", "Noted", true);
        }
        // ADR-0058: rendering reads elements through pure code, so a
        // still-deferred `.map` Seq must run its callback first.
        self.reify_map_grep_seq_args(&values)?;
        // A lone Junction argument to `put` autothreads: each eigenstate is
        // put on its own line (`put 1|2` => "1\n2\n").
        if kind == OutputKind::Put
            && values.len() == 1
            && matches!(values[0].view(), ValueView::Junction { .. })
        {
            let v = self.auto_fetch_proxy(&values[0])?;
            check_rat_divide_by_zero(&v)?;
            let mut lines = Vec::new();
            self.collect_put_lines(&v, &mut lines)?;
            for line in &lines {
                self.write_to_named_handle("$*OUT", line, true)?;
            }
            return Ok(());
        }
        // Slice F: a user `.gist`/`.Str` closure run below can mutate a
        // captured-outer caller lexical (`say $x but role { method gist
        // {$seen=1} }`). Capture the caller frame's code before any dispatch
        // clobbers `current_code`, and reconcile after.
        let caller_code = self.current_code;
        let mut content = String::new();
        for v in &values {
            // `say`/`note` render only the gist head of a long list, so only
            // that head is FETCHed, checked and walked below.
            let v = if kind.renders_gist() {
                runtime::gist_head(v)
            } else {
                v.clone()
            };
            let v = self.resolve_proxies_in_value(&v)?;
            check_rat_divide_by_zero(&v)?;
            check_unhandled_failure(&v)?;
            if kind.renders_gist() {
                // Resolve bound-element sentinels inside arrays before gist.
                let v = self.resolve_bound_array_elements(v);
                if needs_method_dispatch(&v) {
                    content.push_str(&self.render_gist_value(&v)?);
                } else {
                    content.push_str(&runtime::gist_value(&v));
                }
            } else {
                self.collect_str_threaded(&v, &mut content)?;
            }
        }
        self.reconcile_caller_after_internal_dispatch(caller_code);
        self.write_to_named_handle(kind.handle(), &content, kind.appends_newline())
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
            // `print`/`put` stringify via `.Str`, so a `Regex` warns and
            // contributes nothing -- see `regex_str_coercion`. (`say` uses
            // `.gist` and renders the source text with no warning.)
            _ if Self::is_regex_like_value(v) => {
                let coerced = self
                    .regex_str_coercion(v)
                    .expect("is_regex_like_value gates this")?;
                out.push_str(&coerced.to_string_value());
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
