use crate::runtime;
use crate::symbol::Symbol;
use crate::value::{ArrayKind, EnumValue, RuntimeError, Value, ValueView};

pub(crate) mod coercion;
pub(crate) mod collection;
pub(crate) mod collection_gist;
mod dispatch_core_coerce;
pub(crate) mod dispatch_core_math;
mod dispatch_core_numeric;
mod dispatch_core_repr;
pub(crate) mod which;
pub(crate) use crate::value::match_helpers;
pub(crate) use crate::value::raku_repr;
pub(crate) mod temporal;
pub(crate) mod temporal_dispatch;

use crate::value::ValueMap;

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
    // A `Match` answers `Any`'s `self.list.METHOD` methods on its positional
    // captures (ADR-11276 §9.98), as the interpreter's entries do
    // (`Interpreter::match_list_receiver`, which also asks the grammar).
    // Cost: O(1) unless the receiver is a Match and the name one of those.
    if let Some(list) = target.match_list_receiver(method_sym.as_str()) {
        if crate::value::match_view::match_list_answers_itself(method_sym.as_str()) {
            return Some(Ok(list));
        }
        return native_method_0arg(&list, method_sym);
    }
    native_method_0arg_cascade(target, method_sym)
}

/// The row a plain `Match` reaches for `method` (its own, then `Cool`'s
/// audited text rows and `Mu`'s and `Any`'s audited ones), invoked on a match
/// the table has no shape for (a grammar cursor, an instance of a `Match`
/// subclass): their structure is a `Match`'s, so every such row reads them as
/// it reads a plain match.
// Cost: O(1) to find the row, plus the row's own cost.
pub(crate) fn match_owner_row(
    target: &Value,
    method: Symbol,
    args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    // `Mu.perl` is `.raku`.
    let method = if method == "perl" {
        Symbol::intern("raku")
    } else {
        method
    };
    crate::builtins::method_table::answer_as(
        crate::value::DispatchShape::Match,
        target,
        method,
        args,
    )
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

    // A method call decontainerizes its invocant, so an itemized Hash receiver
    // (a `$`-held hash, an element read out of an Array/Hash, `.item`, `$(%h)`)
    // is just the hash here: `$s.cache` / `.permutations` operate on its Pairs,
    // not on the hash as ONE item (#10744). Every VM call op and the interpreter
    // reach this one entry, so it is settled once rather than per op. The flag
    // is cleared over the SAME `HashData` `Gc`; `raku`/`perl`/`item`/`self`
    // observe the container itself (as in the Array arm of
    // `Interpreter::call_method_with_values`) and `VAR` is never a native call.
    // Cost: O(1), a tag probe (`hash_is_itemized` is false for every non-Hash).
    if target.hash_is_itemized() && !matches!(method, "raku" | "perl" | "item" | "self" | "VAR") {
        return native_method_0arg(&target.clone().with_hash_itemized(false), method_sym);
    }

    // `Hash`/`Map` and the QuantHashes inherit `reverse`/`unique`/`squish`/
    // `eager`/`Supply`/`minmax` from `Any`, which defines each as
    // `self.list.METHOD` (#10758): on such a receiver the invocant is its
    // list of Pairs. The interpreter entry
    // (`call_method_with_values`) does the same for the methods that take
    // arguments, and checks for a user `augment` first; this is the
    // zero-argument native twin every VM call op reaches.
    // Cost: O(1) for any other receiver or an unlisted method (one `view()`
    // probe); O(e) for a listed method on a hash-like receiver, e = entries.
    if let Some(pairs) = crate::runtime::utils::hashlike_receiver_as_pairs_list(target, method) {
        return native_method_0arg(&pairs, method_sym);
    }

    // A loop `Label` (`FOO.name`, `FOO.next`, ...); see `builtins/label.rs`.
    // Cost: O(1) to decline any other receiver (one `view()` probe).
    if let Some(result) = crate::builtins::label::label_method_0arg(target, method) {
        return Some(result);
    }

    // Cost: O(1), one lookup in the Attribute metadata map.
    if method == "DEPRECATED"
        && let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = target.view()
        && class_name == "Attribute"
        && let Some(message) = attributes.as_map().get("DEPRECATED")
    {
        return Some(Ok(message.clone()));
    }

    // Cost: O(n), n = bytes in the format string scanned for directives.
    if method == "directives"
        && let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = target.view()
        && class_name == "Format"
    {
        let fmt = attributes
            .as_map()
            .get("format")
            .map(Value::to_string_value)
            .unwrap_or_default();
        let directives = crate::runtime::sprintf::sprintf_arg_specs(&fmt)
            .into_iter()
            .map(|(_, spec)| Value::str(spec.to_string()))
            .collect();
        return Some(Ok(Value::array(directives)));
    }

    // A lazy Match the table has no shape for (a grammar cursor) answers
    // `Match`'s rows here, straight from the capture node, so the probe
    // gauntlet in between (each a `view()`, which would materialize the Match)
    // never runs for the scalar accessors (`from`, `to`, `pos`, `Str`, ...).
    // Cost: O(k) to find the row, k = rows named `method`, plus the row's own.
    if target.is_lazy_match_value() {
        if let Some(result) = match_owner_row(target, method_sym, &[]) {
            return Some(result);
        }
        // `$<>` / `$/<>`: the zen slice of a Match is the Match itself.
        // Cost: O(1).
        if method == "__mutsu_zen_angle" {
            return Some(Ok(target.clone()));
        }
    }

    // ADR-0064: a `.VAR` container descriptor answers only its own properties
    // natively. Every other method is a question about the value the container
    // holds, and only the interpreter can resolve that (a variable descriptor
    // reads the variable's live env entry) -- so defer to the slow path rather
    // than answering out of the descriptor's attribute map.
    if crate::runtime::runtime_var_meta::var_meta_descriptor_defers(target, method) {
        return None;
    }

    // Cost: O(1), one scheduler yield for the calling OS thread.
    if method == "yield" && matches!(target.view(), ValueView::Package(name) if name == "Thread") {
        crate::runtime::thread_usage::note_thread_yield();
        std::thread::yield_now();
        return Some(Ok(Value::NIL));
    }

    // Scalar containers are transparent for method dispatch (except .VAR and
    // .raku/.perl). `.raku`/`.perl` must see the `Scalar` wrapper so an itemized
    // aggregate shows its `$` sigil (`${a=>1}.raku` → `${:a(1)}`); decontainer-
    // izing first would strip it. `.gist` never shows the sigil, so delegating
    // to the inner value is already correct.
    if let ValueView::Scalar(inner) = target.view() {
        if method == "VAR" {
            return Some(Ok(Value::package(crate::symbol::Symbol::intern("Scalar"))));
        }
        if method == "raku" || method == "perl" {
            return Some(Ok(Value::str(raku_repr::raku_value(target))));
        }
        let inner = if inner.is_container_ref() {
            inner.deref_container()
        } else {
            inner.clone()
        };
        return native_method_0arg(&inner, method_sym);
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

    // $!.pending returns a list of all tracked Failure values (S04 spec).
    if method == "pending" {
        let failures = crate::value::get_pending_failures();
        return Some(Ok(Value::array_with_kind(
            crate::gc::Gc::new(crate::value::ArrayData::new(failures)),
            crate::value::ArrayKind::List,
        )));
    }

    // `Failure`'s `exception`, `handled`, `gist`, `raku`, `Str` and `Bool` are rows of
    // the method table (`method_table::failure`, ADR-11276 §9.51). `exception` and
    // `handled` are in the interpreter's no-explode safe list, so answering them
    // here is correct; a non-safe method (`.message`) is not a row, so it reaches
    // the interpreter, which explodes an unhandled Failure.
    if let Some(answer) = crate::builtins::method_table::failure::answer(target, method) {
        return Some(answer);
    }

    // An instance of a user subclass of `Int` answers `Int`'s methods on its
    // payload (`builtins::native_base`).
    if let Some(result) = super::native_base::dispatch(target, method_sym, &[]) {
        return Some(result);
    }

    // For Mixin values, handle Bool/WHICH method specially, then delegate to inner.
    if let ValueView::Mixin(inner, mixins) = target.view() {
        // A plain allomorph's method with a row is the row's (§9.101).
        if crate::builtins::method_table::allomorph::answers(target, method_sym, &[]) {
            return None;
        }
        if method == "Bool"
            && let Some(bool_val) = mixins.get("Bool")
        {
            return Some(Ok(bool_val.clone()));
        }
        if (method == "Str" || method == "~")
            && let Some(str_val) = mixins.get("Str")
        {
            return Some(Ok(str_val.clone()));
        }
        // A Bool with a mixed-in Bool override and no `Str` or role mixin
        // (`True but False`) is that Bool for every method, as in Rakudo
        // (`.Str`, `.Int`, `.not` and `.raku` all follow the effective truth,
        // ADR-11276 §9.111).
        if matches!(inner.as_ref().view(), ValueView::Bool(_))
            && mixins.get("Str").is_none()
            && !mixins.keys().any(|k| k.starts_with("__mutsu_role__"))
            && let Some(ValueView::Bool(b)) = mixins.get("Bool").map(Value::view)
        {
            return native_method_0arg(&Value::truth(b), method_sym);
        }
        // An allomorph (IntStr/NumStr/…) gists as its preserved source string,
        // not the inner numeric's gist: `<1e3>.gist` → `1e3` (not `1000`). Only
        // allomorphs; a general `but`-mixin gists via its inner value (below).
        // A `Str` mixed into any value (`5 but "x"`) wins for the stringifying
        // methods too, as in Rakudo: `.gist` and `.raku`/`.perl` answer it.
        if matches!(method, "gist" | "raku" | "perl")
            && let Some(str_val) = mixins.get("Str")
            && matches!(str_val.view(), ValueView::Str(_))
            && (method == "gist"
                || crate::value::types::allomorph_type_name(inner, mixins).is_none())
        {
            return Some(Ok(str_val.clone()));
        }
        if method == "WHICH"
            && let Some(allo_name) = crate::value::types::allomorph_type_name(inner, mixins)
        {
            let inner_which = match inner.as_ref().view() {
                ValueView::Int(n) => format!("Int|{}", n),
                ValueView::BigInt(n) => format!("Int|{}", *n),
                ValueView::Num(n) => format!("Num|{}", n),
                ValueView::Rat(n, d) => format!("Rat|{}/{}", n, d),
                ValueView::FatRat(n, d) => format!("FatRat|{}/{}", n, d),
                ValueView::BigRat(n, d) => {
                    let flavour = if inner.as_ref().is_bigfatrat() {
                        "FatRat"
                    } else {
                        "Rat"
                    };
                    format!("{}|{}/{}", flavour, n, d)
                }
                ValueView::Complex(r, i) => format!("Complex|{}+{}i", r, i),
                _ => format!("{:?}", inner),
            };
            let str_part = mixins
                .get("Str")
                .map(|v| v.to_string_value())
                .unwrap_or_default();
            let which_str = format!("{}|{}|Str|{}", allo_name, inner_which, str_part);
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("WHICH".to_string(), Value::str(which_str));
            return Some(Ok(Value::make_instance(
                crate::symbol::Symbol::intern("ValueObjAt"),
                attrs,
            )));
        }
        // An allomorph's `wordcase` reads the Str part but preserves the
        // allomorph type (see `allomorph_wordcase_result`); its other string
        // methods are `Str`'s and `Cool`'s rows on the Str part (§9.101).
        if method == "wordcase"
            && let Some(str_val) = mixins.get("Str")
        {
            let wordcased = crate::value::wordcase_str(&str_val.to_string_value());
            return Some(Ok(crate::value::types::allomorph_wordcase_result(
                inner, wordcased,
            )));
        }
        // An allomorph (IntStr/RatStr/NumStr/ComplexStr) renders its `.raku` /
        // `.perl` as `TypeStr.new(<numeric>, "<string>")`, NOT the bare inner
        // value's `.raku`. (`.gist`/`.Str` keep the source-string form, handled
        // above; a general `but`-mixin falls through to the inner delegation.)
        if matches!(method, "raku" | "perl")
            && crate::value::types::allomorph_type_name(inner, mixins).is_some()
        {
            return Some(Ok(Value::str(raku_repr::raku_value(target))));
        }
        // A Set/Bag/Mix names its own type in `.raku` and `.gist`, so a
        // `but`-mixed one has to name the role with it (`Set+{R}.new("a")`,
        // `Set+{R}(a)`). Delegating to the inner value below renders the bare
        // `Set`, dropping the role the way `^name` would without its own arm
        // just above. Only the wrapper knows the mixed name, so pass it down.
        if matches!(method, "raku" | "perl" | "gist")
            && crate::value::role_mixin_suffix(mixins).is_some()
        {
            let type_name = crate::value::what_type_name(target);
            let rendered = if method == "gist" {
                crate::runtime::utils::setbagmix_gist_named(inner, Some(&type_name))
            } else {
                raku_repr::setbagmix_raku_named(inner, Some(&type_name))
            };
            if let Some(rendered) = rendered {
                return Some(Ok(Value::str(rendered)));
            }
        }
        // A type object (a bare `Package`, e.g. `Any but role Meows {...}`)
        // names its own composed type in `.gist`/`.raku`/`.perl`, the same
        // way a Set/Bag/Mix does just above — but unlike an INSTANCE (which
        // reaches a composed-name-aware retargeting step further down the
        // slow path, in `methods_call_dispatch.rs`, when this fast path
        // returns `None`), a bare type object's `inner` here IS the terminal
        // value (there's no user-class dispatch below it to fall through
        // to), so without this arm it fell straight to `native_method_0arg
        // (inner, ...)` at the bottom of this function and rendered the
        // plain base type, silently dropping the mixin (`(Any but role
        // Meows{}).gist` was `(Any)`, losing the `+{Meows}` that `.^name`
        // already reports correctly via `what_type_name`).
        if matches!(method, "raku" | "perl" | "gist")
            && matches!(inner.view(), ValueView::Package(_))
            && crate::value::role_mixin_suffix(mixins).is_some()
        {
            // A type object renamed by `.^set_name` renders by that name.
            let composed = mixins
                .get("__mutsu_type_name__")
                .map(Value::to_string_value)
                .unwrap_or_else(|| crate::value::what_type_name(target));
            let rendered = if method == "gist" {
                format!("({composed})")
            } else {
                composed
            };
            return Some(Ok(Value::str(rendered)));
        }
        // `^name` on a role-mixed value is deliberately NOT fast-pathed here
        // (ADR-0060): a rename can live on the composition-keyed shared
        // `.WHAT` node instead of this instance's own `overrides` (e.g.
        // `Hash::Restricted`'s `v.var.WHAT.^set_name(...)`), and this
        // function has no interpreter access to consult that cache. Falling
        // through to `None` (via the `inner` delegation below, which finds
        // no arm for `"^name"` either) reaches `dispatch_caret_name`/
        // `dispatch_classhow_method`'s `"name"` handler, both of which
        // resolve the composition cache correctly.
        // `.clone` on a mixed-in value must PRESERVE the mixin: `(5 but False).clone`
        // stays `Int+{...}` (Bool=False), and a Match — which is modelled as a
        // string carrying its match state as a mixin — must stay a Match. Delegating
        // to the inner value's clone drops the wrapper (and, now that scalar `.clone`
        // is a real method, returns the bare inner instead of falling through to the
        // slow path). Clone the inner and re-apply the mixins here. Role mixins with
        // `:attr(val)` overrides are left to the slow-path handler
        // (`methods_mixin_dispatch`), which threads the override args.
        if method == "clone"
            && !mixins
                .keys()
                .any(|k| k.starts_with("__mutsu_role__") || k.starts_with("__mutsu_attr__"))
        {
            let inner_clone = match native_method_0arg(inner, method_sym) {
                Some(Ok(v)) => v,
                Some(Err(e)) => return Some(Err(e)),
                None => inner.as_ref().clone(),
            };
            return Some(Ok(Value::mixin_with_state(
                inner_clone,
                mixins.as_ref().clone(),
            )));
        }
        // Role mixins need the interpreter-backed clone path below so their
        // private role cell is deep-copied. Delegating this one method to the
        // native inner value would silently drop the wrapper.
        if method == "clone"
            && mixins
                .keys()
                .any(|k| k.starts_with("__mutsu_role__") || k.starts_with("__mutsu_attr__"))
        {
            return None;
        }
        // A role mixed in may parameterize the element or key type
        // (`%h but Associative[Int, Int]`) or declare `of` itself: the
        // interpreter's mixin dispatch reads the composition before it asks
        // the wrapped container's row (ADR-11276 §9.86).
        if matches!(method, "of" | "keyof")
            && mixins.keys().any(|k| k.starts_with("__mutsu_role__"))
        {
            return None;
        }
        // Check for mixin key matching the method name (e.g. "Array", "List", "Int", etc.)
        // This handles `True but [1, 2]` where `.Array` should return the mixed-in array.
        if let Some(mixin_val) = mixins.get(method) {
            return Some(Ok(mixin_val.clone()));
        }
        // `.flat` must run on the WHOLE mixin, not the bare `inner` the
        // fallback below delegates to: `flat_val` (see `flat.rs`'s `Mixin`
        // arm) already knows to flatten through a container inner while
        // preserving a non-container inner's composition, but only if it
        // receives the mixin itself rather than already-unwrapped `inner`.
        if method == "flat" {
            let mut result = Vec::new();
            crate::builtins::flat_val(
                &crate::builtins::deitemize_flat_operand(target),
                &mut result,
                true,
            );
            return Some(Ok(Value::seq(result)));
        }
        // A role mixin over a SCALAR value is itself the single element of its
        // `.list`/`.List`/`.Array` (`(5 but R).list[0].r`): only an aggregate's
        // mixin folds into its elements. The inner's scalar fallback answers
        // `[inner]`, so put the whole mixin back as that one element.
        // Cost: O(1) for the single-element check.
        if matches!(method, "list" | "List" | "Array") {
            let result = native_method_0arg(inner, method_sym);
            if let Some(Ok(res)) = &result
                && let ValueView::Array(items, kind) = res.view()
                && items.len() == 1
                && items
                    .iter()
                    .next()
                    .is_some_and(|e| e.deref_container() == *inner.as_ref())
            {
                return Some(Ok(Value::array_with_kind(
                    crate::gc::Gc::new(crate::value::ArrayData::new(vec![target.clone()])),
                    kind,
                )));
            }
            return result;
        }
        return native_method_0arg(inner, method_sym);
    }
    // A call the method table answers has no arm below this point: the row is
    // the method's only implementation (ADR-11276 §9.88). `native_method_0arg`
    // asked the table before the cascade, so this declines only for the two
    // callers that run the cascade without it: the debug cross-check, which
    // takes a decline as agreement, and the VM entry whose user-override veto
    // refused the row, where the interpreter must answer instead. The probes
    // above stay in front of the rows, so the cross-check still verifies them.
    // Cost: O(1) when no row has the name (a bit test); otherwise a tag probe,
    // one hash lookup and, for a row found, its handler.
    if crate::builtins::method_table::answer(target, method_sym, &[]).is_some() {
        return None;
    }
    // Any.nl-out returns the default newline separator "\n"
    if method == "nl-out" {
        return Some(Ok(Value::str_from("\n")));
    }
    // Native int coercer methods (.byte(), .int8(), .uint16(), etc.). Rakudo
    // declares these on `Cool`, so a `Pair` — which is not Cool — has no such
    // method; answering one here made `.^can('bool')` true for every value and
    // broke callers that probe `.^can($field)` before trying the next
    // candidate. `bool` and the C-width aliases are not coercion methods at all
    // (see `is_native_int_coerce_method`).
    if runtime::native_types::is_native_int_coerce_method(method) && target.isa_check("Cool") {
        // `Range.Numeric` is its element count.
        let elems = crate::builtins::method_table::scalars::numify::numeric_range_elems(target);
        return Some(raku_repr::native_int_coerce_method(
            elems.as_ref().unwrap_or(target),
            method,
        ));
    }
    // Try core string/numeric/array methods first
    if let result @ Some(_) = dispatch_core(target, method) {
        return result;
    }
    // Then type coercion and specialized methods
    coercion::dispatch(target, method)
}

/// Create a X::Cannot::Lazy Failure for .elems on an infinite range.
fn range_elems_lazy_failure(action: &str) -> Option<Result<Value, RuntimeError>> {
    Some(Ok(crate::runtime::utils::cannot_lazy_failure(action)))
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

/// Re-export raku_value for backward compatibility.
pub use raku_repr::raku_value;

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

fn dispatch_core(target: &Value, method: &str) -> Option<Result<Value, RuntimeError>> {
    fn has_date_attrs(attributes: &crate::gc::Gc<crate::value::InstanceAttrs>) -> bool {
        attributes.contains_key("year")
            && attributes.contains_key("month")
            && attributes.contains_key("day")
    }
    fn has_datetime_attrs(attributes: &crate::gc::Gc<crate::value::InstanceAttrs>) -> bool {
        has_date_attrs(attributes)
            && attributes.contains_key("hour")
            && attributes.contains_key("minute")
            && attributes.contains_key("second")
            && attributes.contains_key("timezone")
    }

    // `Code.Capture` and `Code.clone` are rows of the method table
    // (`method_table::code`, ADR-12523 slice 5).
    if matches!(method, "Capture" | "clone")
        && let Some(answer) = crate::builtins::method_table::code::pure_answer(target, method)
    {
        return Some(answer);
    }

    // Date/DateTime 0-arg methods of a receiver with no shape (an instance of
    // a subclass): the rows of `Date` / `DateTime` answer it, the instance as
    // invocant (ADR-11276 §9.87). A plain `Date`/`DateTime` was answered by
    // its shape before the cascade.
    // Cost: O(k) to find the row, k = rows named `method`, plus the handler's.
    fn temporal_owner_row(
        target: &Value,
        owner: &str,
        method: &str,
    ) -> Option<Result<Value, RuntimeError>> {
        // `Mu.perl` is `.raku`.
        let method = if method == "perl" { "raku" } else { method };
        let id = crate::builtins::method_table::owner_row(
            Symbol::intern(owner),
            Symbol::intern(method),
            0,
        )?;
        crate::builtins::method_table::invoke(id, target, &[])
    }
    match target.view() {
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if has_datetime_attrs(&attributes) => {
            // A DateTime subclass keeps its class through `.utc` (Rakudo's
            // `utc` is `in-timezone(0)`, a clone); the runtime's temporal
            // dispatch reblesses the result.
            if method == "utc" && class_name != "DateTime" {
                return None;
            }
            if let Some(result) = temporal_owner_row(target, "DateTime", method) {
                return Some(result);
            }
        }
        ValueView::Instance { attributes, .. } if has_date_attrs(&attributes) => {
            if let Some(result) = temporal_owner_row(target, "Date", method) {
                return Some(result);
            }
        }
        _ => {}
    }

    // `Str.AST` — parse the string as Raku source and return its RakuAST tree
    // (ADR-0010). Applies to string values only.
    // `Str.AST` — parse the string as Raku source and return its RakuAST tree.
    // Non-string invocants fall through to normal dispatch.
    if let ValueView::Str(s) = target.view()
        && method == "AST"
    {
        return Some(crate::rakuast::str_dot_ast(&s));
    };

    // RakuAST node accessors (Phase 3): `.condition`, `.expression`, `.args`,
    // `.statements`, etc. return the node's field values. `.gist`/`.raku`/`.^name`
    // are handled elsewhere, so a non-accessor method name falls through.
    if let ValueView::RakuAst(node) = target.view()
        && let Some(v) = crate::rakuast::node_accessor(node, method)
    {
        return Some(Ok(v));
    }

    // `Instant` and `Duration` (`do Real`) answer from the rows of the method
    // table (`method_table::instances::instant`): `Instant.Instant`, `succ`/`pred`,
    // `to-posix`, `DateTime`, `Date`, `tai` and `narrow` have no arm here.

    // `Any.values` is `self.list`, and a buffer's `list` its `Blob` row's.
    if method == "values" && crate::runtime::Interpreter::is_buf_value(target) {
        return crate::builtins::method_table::blob::list(target, &[]);
    }

    // Distribution methods: meta, Str, gist, defined
    if let ValueView::Instance {
        class_name,
        attributes,
        ..
    } = target.view()
        && class_name == "Distribution"
    {
        match method {
            "meta" => {
                return Some(Ok(attributes.as_map().get("$!meta").cloned().unwrap_or(
                    Value::hash_with_data(Value::hash_arc(ValueMap::default())),
                )));
            }
            "Str" | "gist" => {
                return Some(Ok(Value::str(format!("Distribution<{}>", class_name))));
            }
            "defined" => return Some(Ok(Value::TRUE)),
            _ => {}
        }
    }

    // `Signature`'s `arity`, `count`, `params` and `returns` are rows of the method
    // table (`method_table::signature`, ADR-11276 §9.55).
    if matches!(method, "arity" | "count" | "params" | "returns")
        && let Some(answer) = crate::builtins::method_table::signature::answer(target, method)
    {
        return Some(answer);
    }

    // Exception/X:: methods: `message`, `gist`, `Str`, `backtrace`, `resume`, `throw` and
    // `X::AdHoc.payload` are rows of the method table (`method_table::exception`,
    // ADR-11276 §9.39); `line`, `file` and `filename` are mutsu's own accessors.
    if let Some(answer) = crate::builtins::method_table::exception::answer(target, method) {
        return Some(answer);
    }
    // Backtrace and Backtrace::Frame: their own methods are rows of the method
    // table (`method_table::backtrace`, ADR-11276 §9.35); a call the table's
    // entry did not take reaches the same handlers here. `Any`'s
    // `self.list.METHOD` methods were answered on the frames before the call
    // reached here (`Value::match_list_receiver`, §9.111).
    if let Some(answer) = crate::builtins::method_table::backtrace::answer(target, method, &[]) {
        return Some(answer);
    }
    if let ValueView::Instance {
        class_name,
        attributes,
        ..
    } = target.view()
        && class_name == "Backtrace::Frame"
        && method == "name"
    {
        // A mutsu alias of `subname`; Rakudo's frame has no `name`.
        return Some(Ok(attributes
            .as_map()
            .get("subname")
            .cloned()
            .unwrap_or(Value::str(String::new()))));
    }

    // .resume on exception objects
    if method == "resume"
        && let ValueView::Instance { class_name, .. } = target.view()
    {
        let cn = class_name.resolve();
        if cn == "Exception" || cn.starts_with("X::") || cn == "Failure" || cn == "CX::Warn" {
            return Some(Err(RuntimeError::resume_signal()));
        }
    }

    // IO::Path::Parts does Associative, Positional, and Iterable, but Rakudo's
    // inherited fallback methods itemize the object: `.list`/`.List`/`.values`
    // contain self, `.keys` contains 0, `.pairs`/`.kv` use 0 => self, and
    // `.elems` is 1.
    // Its explicit `.flat`/`.Slip`/`.cache`/`.eager` methods still expose the
    // three ordered part Pairs, while `.hash`/`.Hash`/`.Map` expose the parts
    // by name.
    if let ValueView::Instance {
        class_name,
        attributes,
        ..
    } = target.view()
        && class_name.resolve() == "IO::Path::Parts"
    {
        let attrs = attributes.as_map();
        let keys = crate::runtime::io_path_parts_keys();
        let part = |key: &str| attrs.get(key).cloned().unwrap_or(Value::NIL);
        let make_list = |items: Vec<Value>| {
            Value::array_with_kind(
                crate::gc::Gc::new(crate::value::ArrayData::new(items)),
                crate::value::ArrayKind::List,
            )
        };
        let pairs = || -> Vec<Value> {
            // ADR-0021 I2: data-minted pairs default positional.
            keys.iter()
                .map(|k| Value::value_pair(Value::str((*k).to_string()), part(k)))
                .collect()
        };
        match method {
            "hash" | "Hash" | "Map" => {
                let map: ValueMap = keys.iter().map(|k| (k.to_string(), part(k))).collect();
                return Some(Ok(Value::hash(map)));
            }
            "list" | "List" | "Array" => {
                return Some(Ok(make_list(vec![target.clone()])));
            }
            "flat" | "Slip" | "cache" | "eager" => {
                return Some(Ok(make_list(pairs())));
            }
            "keys" => {
                return Some(Ok(make_list(vec![Value::int(0)])));
            }
            "values" => {
                return Some(Ok(make_list(vec![target.clone()])));
            }
            "pairs" => {
                return Some(Ok(make_list(vec![Value::value_pair(
                    Value::int(0),
                    target.clone(),
                )])));
            }
            "kv" => {
                return Some(Ok(make_list(vec![Value::int(0), target.clone()])));
            }
            "elems" => {
                return Some(Ok(Value::int(1)));
            }
            _ => {}
        }
    }

    // Match object methods
    if let ValueView::Instance { .. } = target.view()
        && target.is_match_instance()
    {
        // `Match`'s rows answer a match the table has no shape for (a grammar
        // cursor, an instance of a subclass) through their owner.
        if let Some(result) = match_owner_row(target, Symbol::intern(method), &[]) {
            return Some(result);
        }
        // `Any`'s list methods (`self.list.METHOD`) were answered on the
        // positional captures before the call reached here
        // (`Interpreter::match_list_receiver`, §9.98); every other name is
        // answered on the Match itself, never on its matched string (§9.96):
        // a row of `Mu`, `Any`, `Capture` or `Cool` it reaches, or the generic
        // arms below. `.so` of a zero-width match is True (#9180), and a name
        // Rakudo's `Match` lacks is `X::Method::NotFound`.
    }

    // Kernel type object methods
    // `Kernel.hostname` works on the type object (Sys::Hostname does exactly this).
    // Cost: O(1), reads the process-cached uname(2) result.
    if let ValueView::Package(name) = target.view()
        && name == "Kernel"
        && method == "hostname"
    {
        return Some(Ok(Value::str(
            crate::runtime::io_sysinfo_host::host_info()
                .hostname
                .clone(),
        )));
    }
    if let ValueView::Package(name) = target.view()
        && name == "Kernel"
        && method == "endian"
    {
        return Some(Ok(Value::enum_parts(
            Symbol::intern("Endian"),
            Symbol::intern(if cfg!(target_endian = "little") {
                "LittleEndian"
            } else {
                "BigEndian"
            }),
            EnumValue::Int(if cfg!(target_endian = "little") { 1 } else { 2 }),
            if cfg!(target_endian = "little") { 1 } else { 2 },
        )));
    }

    // A receiver with no shape that is a `Cool` reaches `Cool`'s text rows
    // once the cascade declined it (ADR-11276 §9.79); an instance needs the
    // interpreter's MRO and is answered by the entries that have one.
    dispatch_core_families(target, method).or_else(|| {
        crate::builtins::method_table::answer_shapeless(None, target, Symbol::intern(method), &[])
    })
}

/// The eight method families [`dispatch_core`] ends in, without the
/// receiver-shaped prologue in front of them.
///
/// Each family returns `Option<Option<Result<..>>>`:
///   `None` = method not handled, try the next family;
///   `Some(inner)` = method matched, `inner` is the answer.
fn dispatch_core_families(target: &Value, method: &str) -> Option<Result<Value, RuntimeError>> {
    macro_rules! try_dispatch {
        ($module:ident) => {
            if let Some(result) = $module::dispatch(target, method) {
                return result;
            }
        };
    }

    try_dispatch!(dispatch_core_numeric);
    try_dispatch!(dispatch_core_repr);

    None
}
