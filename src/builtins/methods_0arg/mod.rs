use crate::builtins::method_table::receiver_step::{MixinStep, Step};
use crate::symbol::Symbol;
use crate::value::{ArrayKind, RuntimeError, Value, ValueView};

pub(crate) mod coercion;
pub(crate) mod collection;
pub(crate) mod collection_gist;
mod dispatch_core_coerce;
pub(crate) mod dispatch_core_math;
pub(crate) mod which;
pub(crate) use crate::value::match_helpers;
pub(crate) use crate::value::raku_repr;
pub(crate) mod temporal;
pub(crate) mod temporal_dispatch;

/// Create an X::Multi::NoMatch error for a method called on a type object.
pub(crate) fn make_no_match_error(method_name: &str) -> RuntimeError {
    let msg = format!("Cannot resolve caller {}", method_name);
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    let ex = Value::make_instance(Symbol::intern("X::Multi::NoMatch"), attrs);
    let mut err = RuntimeError::new(msg);
    err.exception = Some(Box::new(ex));
    err
}

/// Normalize Unicode Nd (decimal digit) characters to their ASCII equivalents.
/// Returns `None` if any non-sign, non-digit character is found.
fn normalize_unicode_digits(s: &str) -> Option<String> {
    let mut result = String::with_capacity(s.len());
    let mut has_unicode = false;
    for ch in s.chars() {
        if ch.is_ascii_digit() || ch == '-' || ch == '+' || ch == '_' || ch == '.' {
            result.push(ch);
        } else if ch == '\u{2212}' {
            result.push('-');
            has_unicode = true;
        } else {
            let d = crate::builtins::unicode::unicode_decimal_digit_value(ch)?;
            result.push(char::from_digit(d, 10).unwrap());
            has_unicode = true;
        }
    }
    if has_unicode { Some(result) } else { None }
}

pub(crate) fn parse_raku_int_from_str(s: &str) -> Option<Value> {
    let trimmed = s.trim();
    if trimmed.is_empty() {
        return None;
    }
    // Try normalizing Unicode digits to ASCII before parsing
    if let Some(ascii) = normalize_unicode_digits(trimmed) {
        return parse_raku_int_from_str(&ascii);
    }
    let normalized = trimmed.replace('\u{2212}', "-");
    let (sign, body) = if let Some(rest) = normalized.strip_prefix('-') {
        (-1_i32, rest)
    } else if let Some(rest) = normalized.strip_prefix('+') {
        (1_i32, rest)
    } else {
        (1_i32, normalized.as_str())
    };
    let body_no_underscores = body.replace('_', "");
    if body_no_underscores.is_empty() {
        return None;
    }
    if let Some((radix, digits)) = body_no_underscores
        .strip_prefix("0x")
        .or_else(|| body_no_underscores.strip_prefix("0X"))
        .map(|digits| (16_u32, digits))
        .or_else(|| {
            body_no_underscores
                .strip_prefix("0o")
                .or_else(|| body_no_underscores.strip_prefix("0O"))
                .map(|digits| (8_u32, digits))
        })
        .or_else(|| {
            body_no_underscores
                .strip_prefix("0b")
                .or_else(|| body_no_underscores.strip_prefix("0B"))
                .map(|digits| (2_u32, digits))
        })
        .or_else(|| {
            body_no_underscores
                .strip_prefix("0d")
                .or_else(|| body_no_underscores.strip_prefix("0D"))
                .map(|digits| (10_u32, digits))
        })
    {
        if digits.is_empty() {
            return None;
        }
        let mut n = num_bigint::BigInt::parse_bytes(digits.as_bytes(), radix)?;
        if sign < 0 {
            n = -n;
        }
        return Some(Value::from_bigint(n));
    }

    let signed_no_underscores = if sign < 0 {
        format!("-{}", body_no_underscores)
    } else {
        body_no_underscores
    };
    if let Ok(n) = signed_no_underscores.parse::<num_bigint::BigInt>() {
        return Some(Value::from_bigint(n));
    }

    if let Ok(f) = signed_no_underscores.parse::<f64>()
        && f.is_finite()
    {
        let truncated = f.trunc();
        let digits = format!("{:.0}", truncated);
        if let Ok(n) = digits.parse::<num_bigint::BigInt>() {
            return Some(Value::from_bigint(n));
        }
    }
    None
}

// ── 0-arg method dispatch ────────────────────────────────────────────
/// Try to dispatch a 0-argument method call on a Value.
/// Returns `Some(Ok(..))` / `Some(Err(..))` when handled, `None` to fall through.
pub(crate) fn native_method_0arg(
    target: &Value,
    method_sym: Symbol,
) -> Option<Result<Value, RuntimeError>> {
    // A plain receiver whose method has a row in the built-in method table
    // (ADR-11276) is answered by the row, for every caller of this entry. The
    // row is the method's only implementation: its arm is gone from the
    // cascade.
    // Cost: O(1) to decline (a bit test on the method symbol), otherwise a
    // tag probe and one hash lookup plus the row's handler.
    if let Some(result) = crate::builtins::method_table::answer(target, method_sym, &[]) {
        return Some(result);
    }
    native_method_0arg_cascade(target, method_sym)
}

/// [`native_method_0arg`] without the method table: the name-matching
/// cascade, which `method_table`'s debug cross-check also runs.
pub(crate) fn native_method_0arg_cascade(
    target: &Value,
    method_sym: Symbol,
) -> Option<Result<Value, RuntimeError>> {
    // `as_str`, not `resolve`: the latter is `as_str().to_owned()`, so every
    // native method call heap-allocated a copy of a string the symbol table
    // already owns as `&'static str`. This is the entry point for EVERY
    // zero-argument native method in the interpreter.
    let method: &str = method_sym.as_str();

    // The receiver step every entry runs (ADR-11276 §2.4,
    // `method_table::receiver_step`): a `Scalar` or an itemized `Array`/`Hash`
    // is transparent but for the methods that observe the container, a
    // hash-like receiver and a `Match` answer `Any`'s `self.list.METHOD`
    // methods on their list, an enum member its base's.
    // Cost: O(1) tag probes for a receiver in no such state.
    match crate::builtins::method_table::receiver_step::step(target, method, 0) {
        Some(Step::Redirect(receiver, _)) => return native_method_0arg(&receiver, method_sym),
        Some(Step::Answer(answer)) => return Some(answer),
        Some(Step::Itself) => {
            if let ValueView::Scalar(inner) = target.view() {
                // An itemized lazy sequence renders the prefix it pulls,
                // which only the interpreter can.
                if matches!(inner.view(), ValueView::LazyList(_)) {
                    return None;
                }
                return Some(Ok(Value::str(raku_repr::raku_value(target))));
            }
        }
        None => {}
    }

    // Seq consumed/cached state checks.
    // Only handle operations that are fully dispatched here in native_method_0arg.
    // Do NOT pre-check methods that fall through to the runtime (like "iterator"),
    // because native_method_0arg is called from both the VM and the interpreter,
    // and consuming twice would throw.
    if let ValueView::Seq(items) = target.view() {
        if method == "cache" {
            // .cache marks as cached; handled fully here (the actual cache impl is
            // in the per-method handler below, this just marks state).
            items.mark_cache_requested();
        } else if method == "is-lazy" && items.is_consumed() {
            // Read-only check: throws on consumed Seq but does NOT consume.
            return Some(Err(crate::value::seq_consumed_error()));
        } else if method == "kv"
            && let Some((_, _, true)) = items.peek_io_lines_parts()
        {
            // `.kv` on a not-yet-touched `IO::Handle.lines`/`.words` Seq is
            // itself terminal: `reify_or_consume_seq_target`'s own `"kv"`
            // special case (`vm/vm_helpers_lazy.rs`) already built this
            // exact value — a fresh deferred Seq over the same handle with
            // `IoLines`'s `kv` flag flipped on, so a `for` loop can still
            // stream it one line at a time (`claim_io_lines_for_streaming`).
            // Reading its (still-empty) elements here via `items.to_vec()`
            // and re-running `.kv`'s general positional-index transform
            // would silently produce an EMPTY result instead — self-check
            // and pass it through unchanged, exactly like
            // `dispatch_iterator_method`'s "already an Iterator instance"
            // short-circuit (`roast/S16-filehandles/io_in_for_loops.t`).
            return Some(Ok(target.clone()));
        }
    }

    // An instance of a user subclass of `Int` answers `Int`'s methods on its
    // payload (`builtins::native_base`).
    if let Some(result) = super::native_base::dispatch(target, method_sym, &[]) {
        return Some(result);
    }

    // A `but`-mixed value: the mixin's overrides answer first, any other
    // method is the wrapped value's (`method_table::receiver_step::mixin_step`).
    if let Some(step) = crate::builtins::method_table::receiver_step::mixin_step(target, method_sym)
    {
        return match step {
            MixinStep::Decline => None,
            MixinStep::Answer(answer) => Some(answer),
            MixinStep::Delegate(inner) => native_method_0arg(&inner, method_sym),
        };
    }
    dispatch_core(target, method_sym)
}

/// True for a value whose element count cannot be reported, so numeric/count
/// coercions (`.elems`/`.Int`/`.Numeric`/`.end`/prefix `+`) throw
/// `X::Cannot::Lazy`: a lazy-backed Array or a genuinely-lazy `LazyList`
/// (infinite sequence/closure/pipe or a `lazy`-marked gather).
pub(crate) fn is_lazy_count_source(target: &Value) -> bool {
    match target.view() {
        ValueView::Array(_, kind) => kind.is_lazy(),
        ValueView::LazyList(ll) => ll.is_genuinely_lazy(),
        _ => false,
    }
}

fn is_infinite_endpoint(v: &Value) -> bool {
    match v.view() {
        ValueView::Whatever | ValueView::HyperWhatever => true,
        ValueView::Num(n) => n.is_infinite(),
        ValueView::Rat(n, d) => d == 0 && n != 0,
        ValueView::FatRat(n, d) => d == 0 && n != 0,
        _ => {
            let n = v.to_f64();
            n.is_infinite()
        }
    }
}

/// f64 value of a Range *start* endpoint for emptiness checks. A `Whatever`
/// start is an open lower bound (`*..1` is `-Inf..1`), so it maps to -Inf.
fn range_start_f64(v: &Value) -> f64 {
    match v.view() {
        ValueView::Whatever | ValueView::HyperWhatever => f64::NEG_INFINITY,
        _ => v.to_f64(),
    }
}

/// f64 value of a Range *end* endpoint for emptiness checks. A `Whatever` end
/// is an open upper bound (`1..*` is `1..Inf`), so it maps to +Inf.
fn range_end_f64(v: &Value) -> f64 {
    match v.view() {
        ValueView::Whatever | ValueView::HyperWhatever => f64::INFINITY,
        _ => v.to_f64(),
    }
}

// Cost: O(1), a fixed number of endpoint type probes and comparisons.
pub(crate) fn is_infinite_range(value: &Value) -> bool {
    match value.view() {
        ValueView::Range(start, end)
        | ValueView::RangeExcl(start, end)
        | ValueView::RangeExclStart(start, end)
        | ValueView::RangeExclBoth(start, end) => end == i64::MAX || start == i64::MIN,
        ValueView::GenericRange { start, end, .. } => {
            if !(is_infinite_endpoint(start) || is_infinite_endpoint(end)) {
                return false;
            }
            // A range whose start strictly exceeds its end is empty (e.g.
            // `Inf..0`, `1..-Inf`), not infinite. NaN endpoints make this
            // comparison false, so NaN ranges stay "infinite" (they iterate
            // their NaN/-Inf start ad infinitum).
            let s = range_start_f64(start);
            let e = range_end_f64(end);
            // Empty (not infinite) only when start strictly exceeds end. A NaN
            // endpoint is unordered, so `partial_cmp` is `None` -> not empty ->
            // infinite (NaN ranges iterate their NaN start forever).
            !matches!(s.partial_cmp(&e), Some(std::cmp::Ordering::Greater))
        }
        _ => false,
    }
}

pub(crate) fn is_value_lazy(value: &Value) -> bool {
    // This helper also determines whether `.lazy` can tag a LazyList, so it is
    // intentionally broader than the answer returned by `.is-lazy`.
    matches!(value.view(), ValueView::LazyList(ll) if !ll.is_cat_pull())
        || matches!(value.view(), ValueView::Array(_, kind) if kind.is_lazy())
        || is_infinite_range(value)
        || matches!(value.view(), ValueView::Seq(items) if items.is_lazy())
}

/// Return the gist (compact display) representation of a Range value.
fn range_gist_string(value: &Value) -> String {
    // Range.gist is identical to Range.raku in Rakudo: numeric endpoints render
    // plainly, string endpoints are quoted (`"a".."c"`), `i64::MAX`/Whatever
    // endpoints render as `Inf`/`-Inf`, and `0..^N` uses the `^N` short form.
    // Delegate to the raku renderer so both stay in sync.
    if value.is_range() {
        raku_repr::raku_value(value)
    } else {
        value.to_string_value()
    }
}

fn gist_array_wrap(inner: &str, kind: ArrayKind) -> String {
    // `.gist` never shows the `$` itemization marker — at any nesting level
    // (only `.raku` does). So an itemized array/list gists exactly like its
    // non-itemized counterpart: `$[1,2].gist` → `[1 2]`, `$(1,2).gist` → `(1 2)`.
    match kind {
        ArrayKind::Array | ArrayKind::Shaped | ArrayKind::Lazy | ArrayKind::ItemArray => {
            format!("[{}]", inner)
        }
        ArrayKind::List | ArrayKind::ItemList => format!("({})", inner),
    }
}

/// Re-export complex_trig for external use.
pub(crate) use crate::builtins::method_table::complex_math::complex_trig;

pub(crate) use dispatch_core_coerce::{complex_not_real_error, complex_not_real_exception};
/// Re-export the X::Str::Numeric Failure builder for the VM's prefix-`+` op.
pub(crate) use dispatch_core_coerce::{str_numeric_error, str_numeric_failure};

/// Unicode case folding for `.fc` and `fc()`.
pub(crate) fn unicode_foldcase(s: &str) -> String {
    // Unicode full case folding (CaseFolding.txt, statuses C + F). Most
    // characters fold to their simple lowercase (`char::to_lowercase`); the
    // characters whose full fold expands to several codepoints (ligatures, ß,
    // the Greek iota-subscript vowels, ...) come from `full_case_fold`. A plain
    // NFKD would be wrong here — it also decomposes non-cased compatibility
    // characters (NBSP, superscripts, Roman numerals, circled/fullwidth
    // letters), which do not case-fold to their decomposition.
    let mut out = String::with_capacity(s.len());
    for c in s.chars() {
        match crate::builtins::unicode::full_case_fold(c as u32) {
            Some(folded) => out.push_str(folded),
            None => out.extend(c.to_lowercase()),
        }
    }
    out
}

fn dispatch_core(target: &Value, method_sym: Symbol) -> Option<Result<Value, RuntimeError>> {
    let method = method_sym.as_str();
    // `Code.Capture` and `Code.clone` are rows of the method table
    // (`method_table::code`, ADR-12523 slice 5).
    if matches!(method, "Capture" | "clone")
        && let Some(answer) = crate::builtins::method_table::code::pure_answer(target, method)
    {
        return Some(answer);
    }

    // RakuAST node accessors (Phase 3): `.condition`, `.expression`, `.args`,
    // `.statements`, etc. return the node's field values. `.gist`/`.raku`/`.^name`
    // are handled elsewhere, so a non-accessor method name falls through.
    if let ValueView::RakuAst(node) = target.view()
        && let Some(v) = crate::rakuast::node_accessor(node, method)
    {
        return Some(Ok(v));
    }
    if let ValueView::Instance { attributes, .. } = target.view()
        && let Some(base) = attributes.rakuast_base()
        && let ValueView::RakuAst(node) = base.view()
        && let Some(value) = crate::rakuast::node_accessor(node, method)
    {
        return Some(Ok(value));
    }

    // `Instant` and `Duration` (`do Real`) answer from the rows of the method
    // table (`method_table::instances::instant`): `Instant.Instant`, `succ`/`pred`,
    // `to-posix`, `DateTime`, `Date`, `tai` and `narrow` have no arm here.

    // Exception/X:: methods: `message`, `gist`, `Str`, `backtrace` and
    // `X::AdHoc.payload` are rows of the method table, reached through
    // `method_table::instances::exception::redirect` (ADR-11276 §9.39);
    // `line`, `file` and `filename` are mutsu's own accessors. The control
    // methods (`throw`, `resume`, ...) are interpreter rows (§9.163).
    // A receiver with no shape that is a `Cool` reaches `Cool`'s text rows
    // once the cascade declined it (ADR-11276 §9.79); an instance needs the
    // interpreter's MRO and is answered by the entries that have one.
    crate::builtins::method_table::answer_shapeless(None, target, method_sym, &[])
}
