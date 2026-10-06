use crate::runtime;
use crate::symbol::Symbol;
use crate::value::{ArrayKind, EnumValue, RuntimeError, Value, ValueView};
use num_traits::{Signed, ToPrimitive};

use super::rng::builtin_rand;

pub(crate) mod coercion;
pub(crate) mod collection;
mod dispatch_core_coerce;
mod dispatch_core_list;
pub(crate) mod dispatch_core_math;
mod dispatch_core_numeric;
pub(crate) mod dispatch_core_range;
mod dispatch_core_repr;
mod dispatch_core_str;
mod dispatch_core_unicode;
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

fn sample_weighted_mix_key(items: &crate::value::MixData) -> Option<Value> {
    let mut total = 0.0;
    for weight in items.values() {
        if weight.is_finite() && *weight > 0.0 {
            total += *weight;
        }
    }
    if total <= 0.0 {
        return None;
    }
    let mut needle = builtin_rand() * total;
    for (key, weight) in items.iter() {
        if !weight.is_finite() || *weight <= 0.0 {
            continue;
        }
        if needle <= *weight {
            return Some(items.typed_key(key));
        }
        needle -= *weight;
    }
    items
        .iter()
        .find_map(|(key, weight)| (*weight > 0.0).then(|| items.typed_key(key)))
}

fn sample_weighted_bag_key(items: &crate::value::BagData) -> Option<Value> {
    use crate::runtime::utils::bigint_to_i128_sat;
    let mut total: i128 = 0;
    for count in items.values() {
        let count = bigint_to_i128_sat(count);
        if count > 0 {
            total = total.saturating_add(count);
        }
    }
    if total <= 0 {
        return None;
    }
    let needle_f = builtin_rand() * total as f64;
    let mut needle = needle_f as i128;
    if needle >= total {
        needle = total - 1;
    }
    for (key, count) in items.iter() {
        let count = bigint_to_i128_sat(count);
        if count <= 0 {
            continue;
        }
        if needle < count {
            return Some(items.typed_key(key));
        }
        needle -= count;
    }
    items
        .iter()
        .find_map(|(key, count)| count.is_positive().then(|| items.typed_key(key)))
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

/// Format a single item for 0-arg `.fmt()` on lists.
/// Pairs format as "%s\t%s", other values as "%s".
fn fmt_0arg_item(item: &Value) -> String {
    match item.view() {
        ValueView::Pair(k, v) => {
            runtime::format_sprintf_args("%s\t%s", &[Value::str(k.to_string()), v.clone()])
        }
        ValueView::ValuePair(k, v) => {
            runtime::format_sprintf_args("%s\t%s", &[k.clone(), v.clone()])
        }
        _ => runtime::format_sprintf("%s", Some(item)),
    }
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

    // Lazy-Match scalar fast path: these arms are semantically identical to
    // the Match block far below, but answered here straight from the capture
    // node so the probe gauntlet in between (each a `view()`, which would
    // materialize the Match) never runs. Structural methods (`gist`, `list`,
    // `hash`, `caps`, ...) fall through and materialize as before.
    if target.is_lazy_match_value() {
        match method {
            // Cost: O(1) (`.from`/`.to`/`.pos` read the capture node; `.orig`
            // returns the shared subject Value; `.Str` copies the k matched chars).
            "from" => return Some(Ok(Value::int(match_helpers::match_value_from(target)))),
            "to" => return Some(Ok(Value::int(match_helpers::match_value_to(target)))),
            "pos" => {
                return Some(Ok(Value::int(match_helpers::match_visible_pos(
                    target,
                    target.match_pos().unwrap_or(0),
                ))));
            }
            "Str" => {
                return Some(Ok(target
                    .match_str_value()
                    .unwrap_or_else(|| Value::str(String::new()))));
            }
            "Bool" => return Some(Ok(Value::truth(!target.match_is_failed()))),
            "orig" | "target" => {
                return Some(Ok(target
                    .match_orig()
                    .unwrap_or_else(|| Value::str(String::new()))));
            }
            "ast" | "made" => return Some(Ok(target.match_ast().unwrap_or(Value::NIL))),
            "Capture" | "clone" => return Some(Ok(target.clone())),
            // `$<>` / `$/<>`: the zen slice of a Match is the Match itself.
            // Cost: O(1).
            "__mutsu_zen_angle" => return Some(Ok(target.clone())),
            _ => {}
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

    // Unicode's query methods are class methods over the Unicode data shipped
    // with this runtime. `unicode-normalization` exports the authoritative
    // version tuple for the tables mutsu actually uses.
    //
    // This check must come AFTER the lazy-Match fast path above: `target.view()`
    // forces full materialization of a lazy Match, which is exactly what that
    // fast path exists to avoid. By this point `target.is_lazy_match_value()`
    // has already been ruled out (the fast path returned early for a lazy
    // Match reaching one of its handled methods; for an unhandled method it
    // falls through, but a lazy Match is never `ValueView::Package`, so calling
    // `.view()` here is safe/cheap either way -- see the `Scalar` check right
    // below, which relies on the same "lazy-Match case already handled above"
    // invariant to call `target.view()` unconditionally too).
    if matches!(target.view(), ValueView::Package(name) if name == "Unicode") {
        match method {
            "version" => {
                let (major, minor, patch) = unicode_normalization::UNICODE_VERSION;
                let mut parts = vec![
                    crate::value::VersionPart::Num(i64::from(major)),
                    crate::value::VersionPart::Num(i64::from(minor)),
                ];
                // Rakudo renders Unicode 17.0.0 as `v17.0`: retain a patch
                // component only when the Unicode data has a nonzero patch.
                if patch != 0 {
                    parts.push(crate::value::VersionPart::Num(i64::from(patch)));
                }
                return Some(Ok(Value::version(parts, false, false)));
            }
            // mutsu's strings use grapheme-aware Unicode handling throughout,
            // so it provides the same NFG availability answer as MoarVM.
            "NFG" => return Some(Ok(Value::TRUE)),
            _ => {}
        }
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

    // `.dynamic` on a container VALUE — a literal `[1,2,3]`/`{a=>1}`, or an
    // Array/Hash flowing as a value — is always False: only a dynamic *variable*
    // (`@*a`/`%*h`) is dynamic, and that case is rewritten to `.VAR.dynamic` at
    // compile time (`compile_expr_method_on_var`). Array and Hash carry a
    // container descriptor and so define `.dynamic`; List/Int/Str/... do not
    // (raku throws "No such method" there), so restrict this to real Array kinds
    // (not `List`) and Hash and let everything else fall through.
    if method == "dynamic" {
        match target.view() {
            ValueView::Array(_, kind) if !matches!(kind, crate::value::ArrayKind::List) => {
                return Some(Ok(Value::FALSE));
            }
            ValueView::Hash(_) => return Some(Ok(Value::FALSE)),
            _ => {}
        }
    }

    // `.name` on an Array/Hash VALUE reads its container descriptor: the
    // declaring variable (`my %h` names its container "%h", and the name
    // travels through every pass-by-binding chain -- a `\m` param, `self` in
    // a mixed-in role), or rakudo's "element" for a container no declaration
    // named (`Hash.new`, `[1, 2]`, an unsupplied `@`-param). List has no
    // descriptor and no `.name` (raku: "No such method").
    // Cost: O(1).
    if method == "name" {
        let name = match target.view() {
            ValueView::Array(data, kind)
                if !matches!(
                    kind,
                    crate::value::ArrayKind::List | crate::value::ArrayKind::ItemList
                ) =>
            {
                Some(data.descriptor_name.as_deref().map(str::to_string))
            }
            ValueView::Hash(data) => Some(data.descriptor_name.as_deref().map(str::to_string)),
            _ => None,
        };
        if let Some(name) = name {
            return Some(Ok(Value::str(name.unwrap_or_else(|| "element".into()))));
        }
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

    // Instance with __baggy_data__: delegate Bag-like methods to the inner Bag/Set
    // so that subclasses of Bag/Set (e.g. `my class MyBag is Bag {}`) work correctly.
    if let ValueView::Instance { attributes, .. } = target.view()
        && let Some(inner) = attributes.as_map().get("__baggy_data__")
        && !matches!(
            method,
            "WHAT" | "WHICH" | "raku" | "gist" | "Str" | "perl" | "isa" | "^name"
        )
    {
        return native_method_0arg(inner, method_sym);
    }

    // Cost: O(p), p = number of parts in the Version; `.plus` is O(1), while
    // `.parts` allocates O(p) result storage and `.whatever` scans O(p).
    // Version introspection: `.parts` (list of Int/Str/Whatever parts),
    // `.plus` (trailing `+`), `.whatever` (any `*` part). Used by zef's
    // DependencySpecification version matching.
    // The `Version` rows' handlers (ADR-11276, `method_table::scalars::version`).
    if let ValueView::Version { .. } = target.view() {
        match method {
            "parts" => return crate::builtins::method_table::version::parts(target, &[]),
            "plus" => return crate::builtins::method_table::version::plus(target, &[]),
            "whatever" => return crate::builtins::method_table::version::whatever(target, &[]),
            _ => {}
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

    // Nil absorber for common methods: Nil.message, Nil.payload, etc.
    // In Raku, calling most methods on Nil returns Nil.
    if target.is_nil()
        && matches!(
            method,
            "message" | "payload" | "backtrace" | "exception" | "handled" | "line" | "file"
        )
    {
        return Some(Ok(Value::NIL));
    }

    // Failure safe accessors: `.exception` (the stored exception object) and
    // `.handled` (read of the global handled flag). Both are in the
    // interpreter's no-explode safe list, so dispatching them natively is
    // correct — a *non*-safe method (e.g. `.message`) is not handled here, so it
    // returns `None` and reaches the interpreter, which explodes an unhandled
    // Failure. (The `.handled = ...` setter is the 1-arg form, handled
    // elsewhere; this only covers the 0-arg read.)
    if let ValueView::Instance {
        class_name,
        attributes,
        ..
    } = target.view()
        && class_name == "Failure"
    {
        match method {
            "exception" => {
                return Some(Ok(attributes
                    .as_map()
                    .get("exception")
                    .cloned()
                    .unwrap_or(Value::NIL)));
            }
            "handled" => {
                return Some(Ok(Value::truth(target.is_failure_handled())));
            }
            _ => {}
        }
    }

    // An instance of a user subclass of `Int` answers `Int`'s methods on its
    // payload (`builtins::numeric_subclass`).
    if let Some(result) = super::numeric_subclass::dispatch(target, method_sym, &[]) {
        return Some(result);
    }

    // For Mixin values, handle Bool/WHICH method specially, then delegate to inner.
    if let ValueView::Mixin(inner, mixins) = target.view() {
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
        // A Bool with a mixed-in Bool override (`True but False`) renders its
        // Str/gist/raku from the *effective* boolean — `Bool.Str` is
        // `self ?? 'True' !! 'False'`, so it follows `.Bool`, not the base bool.
        if matches!(inner.as_ref().view(), ValueView::Bool(_))
            && mixins.get("Str").is_none()
            && let Some(ValueView::Bool(b)) = mixins.get("Bool").map(Value::view)
        {
            match method {
                "Str" | "~" | "gist" => {
                    return Some(Ok(Value::str(if b { "True" } else { "False" }.to_string())));
                }
                "raku" | "perl" => {
                    return Some(Ok(Value::str(
                        if b { "Bool::True" } else { "Bool::False" }.to_string(),
                    )));
                }
                _ => {}
            }
        }
        // An allomorph (IntStr/NumStr/…) gists as its preserved source string,
        // not the inner numeric's gist: `<1e3>.gist` → `1e3` (not `1000`). Only
        // allomorphs; a general `but`-mixin gists via its inner value (below).
        if method == "gist"
            && crate::value::types::allomorph_type_name(inner, mixins).is_some()
            && let Some(str_val) = mixins.get("Str")
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
        // For allomorphic types, string-oriented methods should use the Str part.
        if let Some(str_val) = mixins.get("Str") {
            match method {
                // `ord`/`ords` belong here for the same reason as `comb`: they
                // read the *characters*, so on an allomorph they must read the
                // Str part. Without them the generic mixin delegation below
                // handed them the inner NUMBER, so `IntStr.new(0, "zero").ords`
                // was `(48,)` — the codepoint of "0" — instead of "zero"'s.
                "comb" | "chars" | "codes" | "words" | "lines" | "chomp" | "chop" | "trim"
                | "trim-leading" | "trim-trailing" | "uc" | "lc" | "tc" | "tclc" | "fc"
                | "flip" | "ord" | "ords" | "samemark" | "samespace" | "uniname" | "uninames"
                | "unival" | "univals" | "uniprop" | "uniprops" | "uniparse" | "parse-names"
                | "NFC" | "NFD" | "NFKC" | "NFKD" | "encode" => {
                    return native_method_0arg(str_val, method_sym);
                }
                // `wordcase` reads the Str part like its siblings above, but
                // unlike them it preserves the allomorph type — see
                // `allomorph_wordcase_result` for why.
                "wordcase" => {
                    let wordcased = crate::value::wordcase_str(&str_val.to_string_value());
                    return Some(Ok(crate::value::types::allomorph_wordcase_result(
                        inner, wordcased,
                    )));
                }
                _ => {}
            }
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
        return native_method_0arg(inner, method_sym);
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
        return Some(raku_repr::native_int_coerce_method(target, method));
    }
    // Uni types: the rows of `builtins::method_table::uni` answer `elems`, `codes`,
    // `Int`, `Numeric`, `Str`, `list`, `gist`, `raku` and the positional
    // subscript, and the table is asked first; the cascade reaches a `Uni` only
    // when called without it (the debug cross-check), and then calls the rows'
    // handlers. `.chars`, `.comb` and `.perl` (the deprecated alias of `.raku`)
    // have no row.
    if let ValueView::Uni(u) = target.view() {
        use crate::builtins::method_table::uni;
        match method {
            // `chars` is no `Uni` method in current Rakudo, but roast pins it as the
            // codepoint count (`S15-string-types/NF-types.t`: `NFC.chars`).
            "chars" | "elems" | "codes" | "Int" | "Numeric" => {
                return Some(uni::elems(target, &[]));
            }
            "Str" => return Some(uni::str(target, &[])),
            "list" => return Some(uni::list(target, &[])),
            "gist" => return Some(uni::gist(target, &[])),
            "raku" => return Some(uni::raku(target, &[])),
            "perl" => {
                return Some(Ok(Value::str(raku_repr::uni_raku_repr(&u.text(), &u.form))));
            }
            "comb" => {
                let parts: Vec<Value> = u
                    .text()
                    .chars()
                    .map(|c| Value::str(c.to_string()))
                    .collect();
                return Some(Ok(Value::seq(parts)));
            }
            _ => {}
        }
    }
    // CompUnit::DependencySpecification methods
    if let ValueView::CompUnitDepSpec { short_name } = target.view() {
        return match method {
            "short-name" => Some(Ok(Value::str(short_name.resolve()))),
            "version-matcher" => Some(Ok(Value::TRUE)),
            "auth-matcher" => Some(Ok(Value::TRUE)),
            "api-matcher" => Some(Ok(Value::TRUE)),
            "Str" | "gist" => Some(Ok(Value::str(short_name.resolve()))),
            _ => None,
        };
    }
    // Capture methods
    if let ValueView::Capture { positional, named } = target.view()
        && let result @ Some(_) = dispatch_capture(target, positional, named, method)
    {
        return result;
    }
    // Try core string/numeric/array methods first
    if let result @ Some(_) = dispatch_core(target, method) {
        return result;
    }
    // Then collection methods (keys, values, kv, pairs, etc.)
    if let result @ Some(_) = collection::dispatch(target, method) {
        return result;
    }
    // Then type coercion and specialized methods
    coercion::dispatch(target, method)
}

fn dispatch_capture(
    target: &Value,
    positional: &[Value],
    named: &ValueMap,
    method: &str,
) -> Option<Result<Value, RuntimeError>> {
    // Capture `.keys`/`.values`/`.kv`/`.pairs` interleave the positional
    // part (indexed 0..n) with the named part (raku Capture semantics).
    // These are the `Capture` rows' implementations (`method_table::capture`).
    use crate::builtins::method_table::capture;
    match method {
        "hash" | "Hash" => capture::hash(target, &[]),
        "list" => capture::list(target, &[]),
        "elems" | "Numeric" | "Int" => capture::elems(target, &[]),
        "is-lazy" => Some(Ok(Value::FALSE)),
        "keys" => capture::keys(target, &[]),
        "values" => capture::values(target, &[]),
        "kv" => capture::kv(target, &[]),
        "pairs" => capture::pairs(target, &[]),
        "antipairs" => capture::antipairs(target, &[]),
        "raku" | "perl" => {
            let mut parts = Vec::new();
            for v in positional {
                match v.view() {
                    ValueView::Pair(k, val) => {
                        parts.push(format!(
                            "{} => {}",
                            raku_repr::raku_value(&Value::str(k.clone())),
                            raku_repr::raku_value(val)
                        ));
                    }
                    ValueView::ValuePair(k, val) => {
                        parts.push(format!(
                            "{} => {}",
                            raku_repr::raku_value(k),
                            raku_repr::raku_value(val)
                        ));
                    }
                    _ => parts.push(raku_repr::raku_value(v)),
                }
            }
            let mut named_keys: Vec<&String> = named.keys().collect();
            named_keys.sort();
            for k in named_keys {
                let v = &named[k];
                if let ValueView::Bool(true) = v.view() {
                    parts.push(format!(":{}", k));
                } else if let ValueView::Bool(false) = v.view() {
                    parts.push(format!(":!{}", k));
                } else {
                    parts.push(format!(":{}({})", k, raku_repr::raku_value(v)));
                }
            }
            Some(Ok(Value::str(format!("\\({})", parts.join(", ")))))
        }
        "gist" => Some(Ok(Value::str(crate::value::capture_text::capture_gist(
            positional, named,
        )))),
        "Str" => Some(Ok(Value::str(crate::value::capture_text::capture_str(
            positional, named,
        )))),
        "Bool" => Some(Ok(Value::truth(
            !positional.is_empty() || !named.is_empty(),
        ))),
        "WHAT" => Some(Ok(Value::package(Symbol::intern("Capture")))),
        "flat" => {
            let cap = Value::capture(positional.to_vec(), named.clone());
            Some(Ok(Value::seq(vec![cap])))
        }
        "Seq" | "List" => {
            let cap = Value::capture(positional.to_vec(), named.clone());
            Some(Ok(Value::seq(vec![cap])))
        }
        _ => None,
    }
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

/// Format a numeric value for Instant/Duration .raku output.
/// Always includes a decimal point (e.g. "42.0", "-400.2").
fn format_temporal_num(f: f64) -> String {
    if f.is_nan() {
        return "NaN".to_string();
    }
    if f.is_infinite() {
        return if f > 0.0 {
            "Inf".to_string()
        } else {
            "-Inf".to_string()
        };
    }
    // For values outside the safe i64 Rat range, emit scientific notation
    // so that Str -> Num literal round-trips through the parser instead of
    // overflowing the Rat literal parser.
    if f.is_finite() && f.abs() >= 1e18 {
        return format!("{:e}", f);
    }
    let s = format!("{}", f);
    if s.contains('.') {
        s
    } else {
        format!("{}.0", s)
    }
}

use crate::builtins::backtrace_methods::{
    frame_is_routine as backtrace_frame_is_routine, frame_str as backtrace_frame_str,
};

/// Re-export raku_value for backward compatibility.
pub use raku_repr::raku_value;

/// Re-export complex_trig for external use.
pub(crate) use crate::builtins::method_table::complex_math::complex_trig;

/// Re-export the X::Str::Numeric Failure builder for the VM's prefix-`+` op.
pub(crate) use dispatch_core_coerce::str_numeric_failure;
pub(crate) use dispatch_core_coerce::{complex_not_real_error, complex_not_real_exception};

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

    // Date/DateTime 0-arg methods
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
            if let Some(result) =
                temporal_dispatch::datetime_method_0arg(&(attributes).as_map(), method)
            {
                return Some(result);
            }
        }
        ValueView::Instance { attributes, .. } if has_date_attrs(&attributes) => {
            if let Some(result) =
                temporal_dispatch::date_method_0arg(&(attributes).as_map(), method)
            {
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

    // Instant.Instant returns self (identity coercion)
    if method == "Instant" {
        match target.view() {
            ValueView::Instance { class_name, .. } if class_name == "Instant" => {
                return Some(Ok(target.clone()));
            }
            ValueView::Package(name) if name == "Instant" => {
                return Some(Ok(target.clone()));
            }
            _ => {}
        }
    }

    // `Instant`/`Duration` do `Real`: `succ`/`pred` step by one second.
    if matches!(method, "succ" | "pred")
        && let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = target.view()
        && (class_name == "Instant" || class_name == "Duration")
    {
        return Some(temporal_dispatch::real_role_step(
            class_name,
            attributes.to_map(),
            method == "succ",
        ));
    }

    // Instant methods: to-posix, DateTime, Date, tai
    if let ValueView::Instance {
        class_name,
        attributes,
        ..
    } = target.view()
    {
        if class_name == "Instant" {
            use crate::builtins::methods_0arg::temporal;
            match method {
                "to-posix" => {
                    let val = attributes
                        .as_map()
                        .get("value")
                        .cloned()
                        .unwrap_or(Value::int(0));
                    let tai = crate::runtime::to_float_value(&val).unwrap_or(0.0);
                    let tai_int = tai.floor() as i64;
                    let mut is_leap = false;
                    for &(threshold, cumulative) in temporal::LEAP_SECONDS.iter().skip(1) {
                        if tai_int == threshold + (cumulative - 1) {
                            is_leap = true;
                            break;
                        }
                    }
                    let posix = temporal::instant_to_posix(tai);
                    let posix_val = if posix == posix.floor() {
                        Value::int(posix as i64)
                    } else {
                        Value::num(posix)
                    };
                    return Some(Ok(Value::array(vec![posix_val, Value::truth(is_leap)])));
                }
                "DateTime" => {
                    let val = attributes
                        .as_map()
                        .get("value")
                        .cloned()
                        .unwrap_or(Value::int(0));
                    let (tai_int, tai_frac) = match val.view() {
                        ValueView::Rat(n, d) if d != 0 => (n / d, (n % d) as f64 / d as f64),
                        _ => {
                            let f = crate::runtime::to_float_value(&val).unwrap_or(0.0);
                            (f.floor() as i64, f - f.floor())
                        }
                    };
                    let (y, m, d, h, mi, s) =
                        temporal::instant_to_datetime_leap_aware_parts(tai_int, tai_frac, 0);
                    return Some(Ok(temporal::make_datetime(y, m, d, h, mi, s, 0)));
                }
                "Date" => {
                    let val = attributes
                        .as_map()
                        .get("value")
                        .cloned()
                        .unwrap_or(Value::int(0));
                    let tai = crate::runtime::to_float_value(&val).unwrap_or(0.0);
                    let posix = temporal::instant_to_posix(tai);
                    let (y, m, d) = temporal::epoch_days_to_civil((posix / 86400.0).floor() as i64);
                    return Some(Ok(temporal::make_date(y, m, d)));
                }
                "tai" => {
                    return Some(Ok(attributes
                        .as_map()
                        .get("value")
                        .cloned()
                        .unwrap_or(Value::int(0))));
                }
                _ => {}
            }
        }
        if class_name == "Duration" {
            match method {
                "narrow" => {
                    let val = attributes
                        .as_map()
                        .get("value")
                        .cloned()
                        .unwrap_or(Value::num(0.0));
                    // Rakudo's Duration always holds a Rat, and every
                    // constructor here stores one (`arith::tai_rat`, #11273);
                    // a Num can only come from a hand-built instance, which
                    // narrows through the same Num -> Rat conversion `.Rat`
                    // uses.
                    let val = match val.view() {
                        ValueView::Num(f) if f.is_finite() => {
                            crate::builtins::arith::real_to_rat(&val)
                        }
                        _ => val.clone(),
                    };
                    match val.view() {
                        ValueView::Num(f) => return Some(Ok(Value::num(f))),
                        ValueView::Rat(n, d) if d != 0 && n % d == 0 => {
                            return Some(Ok(Value::int(n / d)));
                        }
                        ValueView::Rat(n, d) => return Some(Ok(Value::rat_raw(n, d))),
                        ValueView::Int(i) => return Some(Ok(Value::int(i))),
                        _ => return Some(Ok(val.clone())),
                    }
                }
                "tai" => {
                    return Some(Ok(attributes
                        .as_map()
                        .get("value")
                        .cloned()
                        .unwrap_or(Value::num(0.0))));
                }
                _ => {}
            }
        }
    }

    // Buf/Blob.Str throws X::Buf::AsStr — except `utf8`, whose `.Str`/`.Stringy`
    // decodes (rakudo: `utf8.new(98,117).Str` is "bu", while `Buf`, `Blob` and
    // `Blob[uint8]` all die). Only the type object's own name matters; prefix
    // `~` still dies for utf8 too.
    if (method == "Str" || method == "Stringy")
        && let ValueView::Instance { class_name, .. } = target.view()
        && crate::runtime::Interpreter::is_buf_value(target)
    {
        let cn = class_name.resolve();
        if cn == "utf8"
            && let Some(decoded) = crate::builtins::decode_buf_method(target, Some("utf-8"))
        {
            return Some(decoded);
        }
        return Some(Err(crate::runtime::Interpreter::buf_as_str_error(
            target, method,
        )));
    }

    // Buf/Blob .values and .list return the byte values as integers
    if (method == "values" || method == "list")
        && let ValueView::Instance { attributes, .. } = target.view()
        && crate::runtime::Interpreter::is_buf_value(target)
    {
        return Some(Ok(Value::array(
            crate::value::value_buf::buf_elems_or_empty(&attributes),
        )));
    }

    // CX::Warn methods: message, resume
    if let ValueView::Instance {
        class_name,
        attributes,
        ..
    } = target.view()
        && class_name == "CX::Warn"
    {
        match method {
            "message" => {
                return Some(Ok(attributes
                    .as_map()
                    .get("message")
                    .cloned()
                    .unwrap_or(Value::str(String::new()))));
            }
            "resume" => return Some(Err(RuntimeError::resume_signal())),
            "gist" | "Str" => {
                return Some(Ok(attributes
                    .as_map()
                    .get("message")
                    .cloned()
                    .unwrap_or(Value::str(String::new()))));
            }
            _ => {}
        }
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

    // Cost: O(1), the payload is an attribute lookup.
    if method == "payload"
        && let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = target.view()
        && class_name == "X::AdHoc"
    {
        let attrs = attributes.as_map();
        return Some(Ok(attrs
            .get("payload")
            .or_else(|| attrs.get("message"))
            .cloned()
            .unwrap_or_else(|| Value::str(String::new()))));
    }

    // Exception/X:: methods: gist, Str, message
    if let ValueView::Instance {
        class_name,
        attributes,
        ..
    } = target.view()
    {
        let cn = class_name.resolve();
        if target.instance_is_exception_by_name() {
            match method {
                "gist" => {
                    let bt = attributes
                        .as_map()
                        .get("backtrace")
                        .map(|v| v.to_string_value())
                        .unwrap_or_default();
                    let append_bt = |msg: String| -> String {
                        if bt.is_empty() {
                            msg
                        } else {
                            format!("{}\n{}", msg, bt)
                        }
                    };
                    // A declared-but-undefined `has $.message` is not a message —
                    // rendering it would print the literal `(Any)`. (Such a class
                    // is routed to the interpreter by the render gate in
                    // `try_native_method`; this keeps the arm honest regardless.)
                    if let Some(msg) = attributes
                        .as_map()
                        .get("message")
                        .filter(|v| !v.is_nil() && !matches!(v.view(), ValueView::Package(_)))
                    {
                        let msg_str = msg.to_string_value();
                        if !msg_str.is_empty() {
                            return Some(Ok(Value::str(append_bt(msg_str))));
                        }
                    }
                    if cn == "Exception" {
                        return Some(Ok(Value::str(append_bt(
                            "Unthrown Exception with no message".to_string(),
                        ))));
                    }
                    if cn == "X::AdHoc" {
                        if let Some(payload) = attributes.as_map().get("payload") {
                            let payload_str = payload.to_string_value();
                            if !payload_str.is_empty() {
                                return Some(Ok(Value::str(append_bt(payload_str))));
                            }
                        }
                        return Some(Ok(Value::str(append_bt("Unexplained error".to_string()))));
                    }
                    // Construct message from typed exception attributes
                    if let Some(formatted) =
                        crate::value::exception_message::format_exception_message(
                            &cn,
                            &(attributes).as_map(),
                        )
                    {
                        return Some(Ok(Value::str(append_bt(formatted))));
                    }
                    return Some(Ok(Value::str(append_bt(format!("{} with no message", cn)))));
                }
                "Str" => {
                    if let Some(msg) = attributes
                        .as_map()
                        .get("message")
                        .filter(|v| !v.is_nil() && !matches!(v.view(), ValueView::Package(_)))
                    {
                        let msg_str = msg.to_string_value();
                        if !msg_str.is_empty() {
                            return Some(Ok(Value::str(msg_str)));
                        }
                    }
                    if cn == "Exception" {
                        return Some(Ok(Value::str(format!("Something went wrong in ({})", cn))));
                    }
                    // X::AdHoc carries its text in `payload`, not `message`
                    // (`die "..."` builds one). `.Str` returns that payload,
                    // mirroring `.gist`/`.message`.
                    if cn == "X::AdHoc" {
                        if let Some(payload) = attributes.as_map().get("payload") {
                            let payload_str = payload.to_string_value();
                            if !payload_str.is_empty() {
                                return Some(Ok(Value::str(payload_str)));
                            }
                        }
                        return Some(Ok(Value::str("Unexplained error".to_string())));
                    }
                    // Construct message from typed exception attributes
                    if let Some(formatted) =
                        crate::value::exception_message::format_exception_message(
                            &cn,
                            &(attributes).as_map(),
                        )
                    {
                        return Some(Ok(Value::str(formatted)));
                    }
                    return Some(Ok(Value::str(format!("{} with no message", cn))));
                }
                "message" => {
                    if let Some(msg) = attributes.as_map().get("message") {
                        return Some(Ok(msg.clone()));
                    }
                    if cn == "X::AdHoc"
                        && let Some(payload) = attributes.as_map().get("payload")
                    {
                        return Some(Ok(payload.clone()));
                    }
                    // Construct message from typed exception attributes
                    if let Some(formatted) =
                        crate::value::exception_message::format_exception_message(
                            &cn,
                            &(attributes).as_map(),
                        )
                    {
                        return Some(Ok(Value::str(formatted)));
                    }
                    return Some(Ok(Value::str(String::new())));
                }
                "line" => {
                    if let Some(line) = attributes.as_map().get("line") {
                        return Some(Ok(line.clone()));
                    }
                    return Some(Ok(Value::NIL));
                }
                "file" => {
                    if let Some(file) = attributes.as_map().get("file") {
                        return Some(Ok(file.clone()));
                    }
                    return Some(Ok(Value::NIL));
                }
                // `X::Comp` (compile-time diagnoses like `X::Syntax::*`)
                // exposes `.filename`, not `.file` -- rakudo's actual
                // attribute is `$!filename`. Fall back to `file` for
                // exceptions that only got the older generic attribute
                // populated (kept for symmetry with the `.file` arm above).
                "filename" => {
                    let map = attributes.as_map();
                    if let Some(filename) = map.get("filename").or_else(|| map.get("file")) {
                        return Some(Ok(filename.clone()));
                    }
                    return Some(Ok(Value::NIL));
                }
                // Cost: O(1).
                "backtrace" => {
                    if let Some(bt) = attributes.as_map().get("backtrace") {
                        return Some(Ok(bt.clone()));
                    }
                    return Some(Ok(Value::NIL));
                }
                _ => {}
            }
        }
    }

    // Backtrace methods: .Str, .gist, .list, .elems
    if let ValueView::Instance {
        class_name,
        attributes,
        ..
    } = target.view()
    {
        let cn = class_name.resolve();
        if cn == "Backtrace" {
            match method {
                "Str" | "Stringy" => {
                    let text = attributes
                        .as_map()
                        .get("text")
                        .map(|v| v.to_string_value())
                        .unwrap_or_default();
                    return Some(Ok(Value::str(text)));
                }
                "gist" => {
                    let count = attributes
                        .as_map()
                        .get("frames")
                        .map(|v| crate::runtime::utils::value_to_list(v).len())
                        .unwrap_or(0);
                    let noun = if count == 1 { "frame" } else { "frames" };
                    return Some(Ok(Value::str(format!("Backtrace({} {})", count, noun))));
                }
                "full" => {
                    // .full renders every frame (mutsu tracks no hidden/setting
                    // frames, so this is the frame list verbatim), one per
                    // line: each frame's `.Str` is newline-terminated, exactly
                    // as in Rakudo, so the concatenation is line-separated.
                    let frames = crate::builtins::backtrace_methods::frames_of(&attributes);
                    let mut out = String::new();
                    for frame in &frames {
                        // See the `concise`/`summary` arm: a frame can arrive
                        // inside an element container once a `.grep` over this
                        // backtrace has promoted its slots, and matching
                        // `ValueView::Instance` without dereferencing skips it.
                        let frame = frame.with_deref(|v| v.clone());
                        if let ValueView::Instance { attributes: fa, .. } = frame.view() {
                            out.push_str(&backtrace_frame_str(&fa));
                        }
                    }
                    return Some(Ok(Value::str(out)));
                }
                // `.outer-caller-idx` is deliberately absent here: its
                // `Int $startidx` is mandatory, so a no-argument call must not
                // silently answer for index 0.
                "nice" | "next-interesting-index" => {
                    if let Some(result) =
                        crate::builtins::backtrace_methods::dispatch(&attributes, method, &[])
                    {
                        return Some(result);
                    }
                }
                "list" | "List" | "flat" | "Seq" => {
                    if let Some(frames) = attributes.as_map().get("frames") {
                        return Some(Ok(frames.clone()));
                    }
                    return Some(Ok(Value::array(vec![])));
                }
                // `Backtrace.is-runtime` distinguishes a backtrace captured
                // while *running* from one attached to a compile-time
                // diagnosis. The Backtrace builders (`vm_helpers.rs`) stamp
                // the flag directly; a compile-time backtrace answers False.
                "is-runtime" => {
                    let is_runtime = attributes
                        .as_map()
                        .get("is-runtime")
                        .is_some_and(|v| v.truthy());
                    return Some(Ok(Value::truth(is_runtime)));
                }
                "concise" | "summary" => {
                    let frames = attributes
                        .as_map()
                        .get("frames")
                        .map(crate::runtime::utils::value_to_list)
                        .unwrap_or_default();
                    let want_summary = method == "summary";
                    let mut out = String::new();
                    for frame in &frames {
                        // A frame can arrive inside an element container: a
                        // `.grep` over this backtrace promotes each matched
                        // source slot to a shared cell so a writeback loop can
                        // mutate through it, and that promotion is published on
                        // the frames array itself. Matching `ValueView::Instance`
                        // without dereferencing silently skipped every promoted
                        // frame, so `.summary` after a `.grep` came back empty.
                        let frame = frame.with_deref(|v| v.clone());
                        if let ValueView::Instance { attributes: fa, .. } = frame.view() {
                            let is_routine = backtrace_frame_is_routine(&fa);
                            // concise: only non-hidden, non-setting routines.
                            // summary: non-hidden items that are routines or
                            // non-setting. Native setting frames carry the
                            // explicit marker inserted by the backtrace builder.
                            let is_setting =
                                fa.as_map().get("is-setting").is_some_and(Value::truthy);
                            let keep = if want_summary {
                                is_routine || !is_setting
                            } else {
                                is_routine && !is_setting
                            };
                            if keep {
                                out.push_str(&backtrace_frame_str(&fa));
                            }
                        }
                    }
                    return Some(Ok(Value::str(out)));
                }
                "elems" => {
                    if let Some(frames) = attributes.as_map().get("frames") {
                        let count = crate::runtime::utils::value_to_list(frames).len();
                        return Some(Ok(Value::int(count as i64)));
                    }
                    return Some(Ok(Value::int(0)));
                }
                _ => {}
            }
        } else if cn == "Backtrace::Frame" {
            match method {
                "subname" => {
                    return Some(Ok(attributes
                        .as_map()
                        .get("subname")
                        .cloned()
                        .unwrap_or(Value::str(String::new()))));
                }
                "file" => {
                    return Some(Ok(attributes
                        .as_map()
                        .get("file")
                        .cloned()
                        .unwrap_or(Value::str(String::new()))));
                }
                "line" => {
                    return Some(Ok(attributes
                        .as_map()
                        .get("line")
                        .cloned()
                        .unwrap_or(Value::int(0))));
                }
                // `.raku`/`.gist` are NOT handled here: Rakudo's
                // `Backtrace::Frame` renders both as
                // `Backtrace::Frame.new(file => ..., line => ..., code => ...,
                // subname => ...)` (it has no custom `.gist`, so it falls back
                // to the default `.raku`-shaped one), which needs `&mut self`
                // to recursively render the synthesized `code` object -- see
                // `default_instance_repr`'s `"Backtrace::Frame"` arm
                // (`runtime/methods_instance_ops.rs`).
                "Str" => {
                    return Some(Ok(Value::str(backtrace_frame_str(&attributes))));
                }
                "code" => {
                    // The Code object for this frame. mutsu does not retain the
                    // actual routine, so synthesize a Routine carrying the name
                    // (`.code.name` is the documented use).
                    return Some(Ok(crate::builtins::backtrace_methods::frame_code_value(
                        &attributes,
                    )));
                }
                "name" => {
                    return Some(Ok(attributes
                        .as_map()
                        .get("subname")
                        .cloned()
                        .unwrap_or(Value::str(String::new()))));
                }
                "is-routine" => {
                    return Some(Ok(Value::truth(backtrace_frame_is_routine(&attributes))));
                }
                "is-hidden" => {
                    return Some(Ok(attributes
                        .as_map()
                        .get("is-hidden")
                        .cloned()
                        .unwrap_or(Value::FALSE)));
                }
                "is-setting" => {
                    return Some(Ok(attributes
                        .as_map()
                        .get("is-setting")
                        .cloned()
                        .unwrap_or(Value::FALSE)));
                }
                _ => {}
            }
        }
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

    // .throw on exception objects
    if method == "throw"
        && let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = target.view()
    {
        let cn = class_name.resolve();
        // Only the core exception classes use the fast path. For CX::* we
        // fall through to the slow path, which can inspect the class's
        // composed roles (e.g. X::Control) to decide whether the throw
        // should raise a control exception.
        if cn == "Exception" || cn.starts_with("X::") || cn == "Failure" {
            // Derive the human message the same way `.message`/`.gist`/`.Str`
            // do — an X::AdHoc carries its text in `payload` (not `message`),
            // and a typed exception's message is built from its attributes —
            // rather than the type repr (`X::AdHoc()`) that
            // `target.to_string_value()` would yield.
            // A declared-but-undefined `has $.message` is NOT a message: it is
            // the state a computing `method message` starts from, and rendering
            // it would print the literal `(Any)`. (Such a class is routed to the
            // interpreter by the `throw`/`rethrow` gate in
            // `try_native_method`, which can see the user method; this filter
            // keeps the pure-attribute classes honest too.)
            let msg = attributes
                .as_map()
                .get("message")
                .filter(|v| !v.is_nil() && !matches!(v.view(), ValueView::Package(_)))
                .map(|v| v.to_string_value())
                .filter(|s| !s.is_empty())
                .or_else(|| {
                    if cn == "X::AdHoc" {
                        attributes
                            .as_map()
                            .get("payload")
                            .map(|v| v.to_string_value())
                            .filter(|s| !s.is_empty())
                    } else {
                        None
                    }
                })
                .or_else(|| {
                    crate::value::exception_message::format_exception_message(
                        &cn,
                        &attributes.as_map(),
                    )
                })
                .unwrap_or_else(|| target.to_string_value());
            let mut err = RuntimeError::new(msg);
            err.exception = Some(Box::new(target.clone()));
            return Some(Err(err));
        }
    }

    // Array of Match objects: .to/.from/.ast
    if let ValueView::Array(arr, _) = target.view() {
        match method {
            "to" | "pos" => {
                if let Some(last) = arr.last() {
                    return native_method_0arg(last, Symbol::intern(method));
                }
                return Some(Ok(Value::int(0)));
            }
            "from" => {
                if let Some(first) = arr.first() {
                    return native_method_0arg(first, Symbol::intern(method));
                }
                return Some(Ok(Value::int(0)));
            }
            "ast" => {
                if let Some(last) = arr.last() {
                    return native_method_0arg(last, Symbol::intern("ast"));
                }
                return Some(Ok(Value::NIL));
            }
            _ => {}
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
    if let ValueView::Instance { attributes, .. } = target.view()
        && target.is_match_instance()
    {
        let list_v = target.match_list();
        let named_v = target.match_named();
        match method {
            "from" => {
                return Some(Ok(Value::int(match_helpers::match_value_from(target))));
            }
            "to" => {
                return Some(Ok(Value::int(match_helpers::match_value_to(target))));
            }
            "pos" => {
                return Some(Ok(Value::int(match_helpers::match_visible_pos(
                    target,
                    target.match_pos().unwrap_or(0),
                ))));
            }
            "gist" => {
                // Full Match gist: corner-quoted text plus positional/named
                // sub-captures, ordered by position and nested recursively.
                let gist = crate::runtime::utils::match_gist(&(attributes).as_map(), 0);
                return Some(Ok(Value::str(gist)));
            }
            "Str" => {
                return Some(Ok(target
                    .match_str_value()
                    .unwrap_or_else(|| Value::str(String::new()))));
            }
            "Bool" => {
                // A failed `.subparse` Match is falsy; every other Match is truthy.
                return Some(Ok(Value::truth(!target.match_is_failed())));
            }
            "orig" | "target" => {
                // `.target` is the original string the match ran against; for an
                // ordinary Match it is identical to `.orig`.
                return Some(Ok(target
                    .match_orig()
                    .unwrap_or_else(|| Value::str(String::new()))));
            }
            "raku" | "perl" => {
                return Some(Ok(Value::str(match_helpers::match_raku_repr(
                    &(attributes).as_map(),
                ))));
            }
            "list" => {
                return Some(Ok(target.match_list_view()));
            }
            // Cost: O(p), p = positional captures (copied once into the result).
            // The 0-arg members of `is_capture_list_method` that arrays answer
            // natively: a Match is never coerced to its `.Str` for them.
            "Array" | "List" | "Slip" | "Seq" | "flat" | "cache" | "eager" | "reverse" => {
                let list = target.match_positional_list(method == "Array");
                // `Capture.List` is the positional list itself; a `List`'s own
                // `.List` would materialize an unbound slot's hole as `Nil`.
                if method == "List" {
                    return Some(Ok(list));
                }
                return native_method_0arg(&list, Symbol::intern(method));
            }
            "hash" | "Hash" => {
                let named = if method == "hash" {
                    target.match_named_map()
                } else {
                    target.match_named()
                };
                return Some(Ok(named.unwrap_or_else(|| Value::hash(ValueMap::default()))));
            }
            "keys" => {
                let mut keys = Vec::new();
                if let Some(ValueView::Array(list, _)) = list_v.as_ref().map(Value::view) {
                    for i in 0..list.len() {
                        keys.push(Value::int(i as i64));
                    }
                }
                if let Some(ValueView::Hash(named)) = named_v.as_ref().map(Value::view) {
                    let mut sorted: Vec<&String> = named.keys().collect();
                    sorted.sort();
                    for k in sorted {
                        keys.push(Value::str(k.clone()));
                    }
                }
                return Some(Ok(Value::seq(keys)));
            }
            "values" => {
                let mut vals = Vec::new();
                if let Some(ValueView::Array(list, _)) = list_v.as_ref().map(Value::view) {
                    // A quantified positional capture (`( ... )*` — $0 is an
                    // Array of per-iteration Matches) flattens into the value
                    // list, matching raku's Seq flattening: `$m.values` over
                    // `( <element> ','?)*` yields the group Matches themselves.
                    for v in list.iter() {
                        if let ValueView::Array(inner, _) = v.view() {
                            vals.extend(inner.iter().cloned());
                        } else {
                            vals.push(v.clone().unbound_capture_as_mu());
                        }
                    }
                }
                if let Some(ValueView::Hash(named)) = named_v.as_ref().map(Value::view) {
                    let mut sorted: Vec<(&String, &Value)> = named.iter().collect();
                    sorted.sort_by_key(|(k, _)| (*k).clone());
                    // Same flattening as the positional branch above: a
                    // quantified/multi-match named capture (`<a>*`) renders
                    // as an Array, but `.values` flattens its entries rather
                    // than yielding the Array itself.
                    for (_, v) in sorted {
                        if let ValueView::Array(inner, _) = v.view() {
                            vals.extend(inner.iter().cloned());
                        } else {
                            vals.push(v.clone());
                        }
                    }
                }
                return Some(Ok(Value::seq(vals)));
            }
            "pairs" => {
                // ADR-0021 I2: data-minted pairs default positional.
                let mut pairs = Vec::new();
                if let Some(ValueView::Array(list, _)) = list_v.as_ref().map(Value::view) {
                    for (i, v) in list.iter().enumerate() {
                        pairs.push(Value::value_pair(
                            Value::int(i as i64),
                            v.clone().unbound_capture_as_mu(),
                        ));
                    }
                }
                if let Some(ValueView::Hash(named)) = named_v.as_ref().map(Value::view) {
                    let mut sorted: Vec<(&String, &Value)> = named.iter().collect();
                    sorted.sort_by_key(|(k, _)| (*k).clone());
                    for (k, v) in sorted {
                        pairs.push(Value::value_pair(Value::str(k.clone()), v.clone()));
                    }
                }
                return Some(Ok(Value::seq(pairs)));
            }
            "kv" => {
                let mut kv = Vec::new();
                if let Some(ValueView::Array(list, _)) = list_v.as_ref().map(Value::view) {
                    for (i, v) in list.iter().enumerate() {
                        kv.push(Value::int(i as i64));
                        // A quantified capture's Array flattens after its key
                        // (raku: `$m.kv` over `(\w)*` is `(0, m1, m2)`).
                        if let ValueView::Array(inner, _) = v.view() {
                            kv.extend(inner.iter().cloned());
                        } else {
                            kv.push(v.clone().unbound_capture_as_mu());
                        }
                    }
                }
                if let Some(ValueView::Hash(named)) = named_v.as_ref().map(Value::view) {
                    let mut sorted: Vec<(&String, &Value)> = named.iter().collect();
                    sorted.sort_by_key(|(k, _)| (*k).clone());
                    for (k, v) in sorted {
                        kv.push(Value::str(k.clone()));
                        // A quantified/multi-match named capture's Array
                        // flattens after its key, same as the positional
                        // branch above (raku: `$m.kv` over `<a>*` is
                        // `("a", m1, m2)`, not `("a", [m1, m2])`; zero
                        // matches leave the key with nothing after it).
                        if let ValueView::Array(inner, _) = v.view() {
                            kv.extend(inner.iter().cloned());
                        } else {
                            kv.push(v.clone());
                        }
                    }
                }
                return Some(Ok(Value::seq(kv)));
            }
            "elems" => {
                let count = match list_v.as_ref().map(Value::view) {
                    Some(ValueView::Array(list, _)) => list.len(),
                    _ => 0,
                };
                return Some(Ok(Value::int(count as i64)));
            }
            "ast" | "made" => {
                return Some(Ok(target.match_ast().unwrap_or(Value::NIL)));
            }
            // Cost: O(p), p = chars of the prefix, for a lazy Match (sliced from its
            // shared subject); O(n), n = chars of `.orig`, for a rebuilt eager one.
            "prematch" => {
                if let Some(pre) = target.match_side_text(true) {
                    return Some(Ok(Value::str(pre)));
                }
                if let Some(orig_val) = target.match_orig() {
                    let orig = orig_val.to_string_value();
                    let from = target.match_from().unwrap_or(0).max(0) as usize;
                    let chars: Vec<char> = orig.chars().collect();
                    let pre: String = chars[..from.min(chars.len())].iter().collect();
                    return Some(Ok(Value::str(pre)));
                }
                return Some(Ok(Value::str(String::new())));
            }
            // Cost: O(s), s = chars of the suffix, for a lazy Match (sliced from its
            // shared subject); O(n), n = chars of `.orig`, for a rebuilt eager one.
            "postmatch" => {
                if let Some(post) = target.match_side_text(false) {
                    return Some(Ok(Value::str(post)));
                }
                if let Some(orig_val) = target.match_orig() {
                    let orig = orig_val.to_string_value();
                    let to = target.match_to().unwrap_or(0).max(0) as usize;
                    let chars: Vec<char> = orig.chars().collect();
                    let post: String = chars[to.min(chars.len())..].iter().collect();
                    return Some(Ok(Value::str(post)));
                }
                return Some(Ok(Value::str(String::new())));
            }
            "actions" => {
                return Some(Ok(attributes
                    .as_map()
                    .get("actions")
                    .cloned()
                    .unwrap_or(Value::NIL)));
            }
            "caps" => {
                return Some(Ok(match_helpers::match_caps(&(attributes).as_map())));
            }
            "chunks" => {
                return Some(Ok(match_helpers::match_chunks(&(attributes).as_map())));
            }
            "Capture" => {
                // Match.Capture returns self
                return Some(Ok(target.clone()));
            }
            "clone" => {
                // A Match clones to a Match — it must NOT delegate to its Str
                // (the Cool `_` coercion below), which would drop the match
                // structure and return the bare matched string. A Match is
                // immutable, so a value clone (sharing the attribute storage) is
                // a correct clone that keeps positional/named captures. (Before
                // scalar `.clone` became a real method this happened to work via
                // `Str.clone` falling through to the slow path.)
                return Some(Ok(target.clone()));
            }
            // `Mu.so` / `Mu.not` are `.Bool` and its negation: a successful
            // Match is true even when it matched the empty string. Delegating
            // them to the matched Str (below) answered from `""` instead, so a
            // zero-width match (`/<?before x>/`) was `.so` False (#9180).
            // Cost: O(1).
            "so" => return Some(Ok(Value::truth(target.truthy()))),
            // Cost: O(1).
            "not" => return Some(Ok(Value::truth(!target.truthy()))),
            // A Match has reference identity (`ObjAt.new("Match|<id>")`), not
            // the value identity of its matched string — the Str delegation
            // below would land on `ValueObjAt.new("Str|...")`. Fall through to
            // the generic Instance arm in `dispatch_core_coerce`.
            "WHICH" => {}
            _ => {
                let str_val = Value::str(target.to_string_value());
                return native_method_0arg(&str_val, Symbol::intern(method));
            }
        }
    }

    // Numeric type object .Range methods
    if let ValueView::Package(name) = target.view()
        && method == "Range"
        && matches!(
            name.resolve().as_str(),
            "Real" | "Num" | "Rational" | "Rat" | "FatRat" | "BigRat"
        )
    {
        return Some(Ok(Value::generic_range(
            Value::num(f64::NEG_INFINITY),
            Value::num(f64::INFINITY),
            false,
            false,
        )));
    }
    if let ValueView::Package(name) = target.view()
        && name.resolve() == "Int"
        && method == "Range"
    {
        return Some(Ok(Value::generic_range(
            Value::num(f64::NEG_INFINITY),
            Value::num(f64::INFINITY),
            true,
            true,
        )));
    }
    if let ValueView::Package(name) = target.view()
        && crate::runtime::native_types::is_native_int_type(&name.resolve())
        && method == "Range"
        && let Some((min_big, max_big)) =
            crate::runtime::native_types::native_int_bounds(&name.resolve())
    {
        let min_i64 = min_big.to_i64();
        let max_i64 = max_big.to_i64();
        if let (Some(min_v), Some(max_v)) = (min_i64, max_i64) {
            return Some(Ok(Value::range(min_v, max_v)));
        } else {
            let min_val = min_i64
                .map(Value::int)
                .unwrap_or_else(|| Value::bigint(min_big));
            let max_val = max_i64
                .map(Value::int)
                .unwrap_or_else(|| Value::bigint(max_big));
            return Some(Ok(Value::generic_range(min_val, max_val, false, false)));
        }
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

    dispatch_core_families(target, method)
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

    try_dispatch!(dispatch_core_coerce);
    try_dispatch!(dispatch_core_unicode);
    try_dispatch!(dispatch_core_numeric);
    try_dispatch!(dispatch_core_list);
    try_dispatch!(dispatch_core_str);
    try_dispatch!(dispatch_core_repr);
    try_dispatch!(dispatch_core_range);
    try_dispatch!(dispatch_core_math);

    None
}
