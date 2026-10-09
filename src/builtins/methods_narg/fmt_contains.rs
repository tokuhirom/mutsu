use super::str_match::is_str_or_match_receiver;
use crate::runtime;
use crate::value::{RuntimeError, Value, ValueView};

/// Whether formatting `v` through a `.fmt()`/`sprintf` numeric or `%s`
/// directive might need to dispatch a user-defined `.Str`/`.Int`/`.Numeric`
/// coercion method — i.e. `v` is something the pure `format_sprintf`/
/// `format_sprintf_args` formatter cannot itself resolve. That formatter's
/// numeric extractors match only the built-in `ValueView` numeric/string
/// variants and silently fall back to 0/"" for anything else (see
/// `runtime::sprintf`), so an `Instance`/`Package`/role-mixin argument must
/// be routed to the interpreter-aware slow path
/// (`Interpreter::dispatch_fmt_with_user_coercion`) first.
pub(crate) fn fmt_value_needs_coercion(v: &Value) -> bool {
    matches!(v.view(), ValueView::Instance { .. } | ValueView::Package(_)) || v.is_mixin_value()
}

pub(crate) fn fmt_joinable_target(target: &Value) -> bool {
    matches!(
        target.view(),
        ValueView::Array(..)
            | ValueView::Seq(..)
            | ValueView::Slip(..)
            | ValueView::Range(..)
            | ValueView::RangeExcl(..)
            | ValueView::RangeExclStart(..)
            | ValueView::RangeExclBoth(..)
            | ValueView::GenericRange { .. }
    )
}

/// Extract key and value from a Pair or ValuePair.
pub(crate) fn pair_key_value(val: &Value) -> Option<(Value, Value)> {
    match val.view() {
        ValueView::Pair(k, v) => Some((Value::str(k.to_string()), v.clone())),
        ValueView::ValuePair(k, v) => Some((k.clone(), v.clone())),
        _ => None,
    }
}

/// Format one item of a `List.fmt` for `.fmt()`. As in Rakudo, every item
/// (a Pair included) is one `sprintf` argument, and a directive count other
/// than one raises a plain `X::AdHoc` carrying the count message.
// Cost: O(f + n), f = chars of the format, n = chars of the rendered value.
pub(crate) fn fmt_list_item(fmt: &str, item: &Value) -> Result<String, RuntimeError> {
    validate_list_item_directives(fmt)?;
    Ok(runtime::format_sprintf(fmt, Some(item)))
}

/// The one-argument directive check of [`fmt_list_item`].
// Cost: O(f), f = chars of the format.
pub(crate) fn validate_list_item_directives(fmt: &str) -> Result<(), RuntimeError> {
    runtime::sprintf::validate_sprintf_directives(fmt, 1).map_err(|e| {
        let is_count = e.exception.as_ref().is_some_and(|x| {
            matches!(x.view(), ValueView::Instance { class_name, .. }
                if class_name.resolve() == "X::Str::Sprintf::Directives::Count")
        });
        if is_count {
            // Rakudo's `List.fmt` dies with this short, unwrapped message.
            let first = e.message.lines().next().unwrap_or_default();
            RuntimeError::new(format!("{first} supplied"))
        } else {
            e
        }
    })
}

/// `.contains($needle, $pos?, :i/:ignorecase/:m/:ignoremark?)` on a `Str` receiver —
/// the forms that carry a start position and/or the case-/mark-insensitive named
/// markings. These never reach the arity-keyed `native_method_*arg` dispatch (a Pair
/// or a 3rd arg pushes them past it), so they previously bounced to the interpreter.
/// Both it and `Interpreter::dispatch_contains` call `str_prim::contains`, which
/// folds `:i`/`:m` the way `nqp::indexic`/`indexim` do; the start position is taken
/// from the second positional (Int/Num/Str-parsed).
///
/// Returns `None` (fall through to the interpreter) for: non-Str receivers, a Package
/// (type-object) needle, a `BigInt` position (overflow → X::OutOfRange handled by the
/// interpreter), and out-of-range / negative positions (X::OutOfRange Failure). The
/// plain single-needle form (`contains($needle)`) keeps its `native_method_1arg` arm.
// Cost: O(d * m) amortized, d = chars searched from `$pos`, m = chars of the
// needle, once the invocant's grapheme index is cached (`$pos` is resolved
// through it). :i/:m fold the searched graphemes, O(d) extra.
pub(crate) fn native_contains_with_options(
    target: &Value,
    args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    if !is_str_or_match_receiver(target) {
        return None;
    }
    let mut positional: Vec<&Value> = Vec::new();
    let mut ignore_case = false;
    let mut ignore_mark = false;
    for arg in args {
        if let ValueView::Pair(key, value) = arg.view() {
            match key.as_str() {
                "i" | "ignorecase" => ignore_case = value.truthy(),
                "m" | "ignoremark" => ignore_mark = value.truthy(),
                // An unexpected named arg: let the interpreter own the semantics.
                _ => return None,
            }
        } else {
            positional.push(arg);
        }
    }
    // Only the positioned / named forms are handled here; the bare single needle
    // (`contains($needle)`) stays on the existing 1-arg native arm.
    let needle: &Value = positional.first().copied()?;
    if positional.len() == 1 && args.len() == 1 {
        return None;
    }
    if let ValueView::Package(_) = needle.view() {
        return None;
    }
    // A Regex needle needs the regex engine (&mut self); let the interpreter's
    // dispatch_contains own it rather than searching for the regex's gist.
    if let ValueView::Regex(..) = needle.view() {
        return None;
    }
    let start = match positional.get(1).copied().map(Value::view) {
        Some(ValueView::Int(i)) => i,
        Some(ValueView::Num(f)) => f as i64,
        Some(ValueView::Str(s)) => s.parse::<i64>().ok()?,
        Some(_) => return None,
        None => 0,
    };
    if start < 0 {
        return None;
    }
    // The position is a grapheme index, resolved through the cached index
    // rather than by re-collecting the suffix (#9140).
    crate::builtins::grapheme_index::with_str_index(target, |text, idx| {
        if start as usize > idx.len() {
            return None;
        }
        let fold = crate::builtins::str_prim::Fold::new(ignore_case, ignore_mark);
        Some(Ok(crate::builtins::str_prim::contains(
            text,
            idx,
            start as usize,
            needle,
            fold,
        )))
    })
}

/// Format one item for 0-arg `.fmt()` on lists: a Pair formats as `%s\t%s`,
/// any other value as `%s`.
// Cost: O(n), n = chars of the rendered item.
fn fmt_default_item(item: &Value) -> String {
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

/// `.fmt` on a collection or a scalar, for the three forms `fmt`, `fmt($format)`
/// and `fmt($format, $separator)`: the one implementation the `fmt` rows
/// (`method_table::collections::fmt`) and the 0-, 1- and 2-argument cascades share
/// (ADR-11276, the rendering names).
///
/// The items are formatted by `format_sprintf`, which cannot dispatch a user
/// `.Str`/`.Int`/`.Numeric`, so a `$format` with a directive over an item that may
/// carry one (`fmt_value_needs_coercion`) and a `Format` object as `$format`
/// decline (`None`) to the interpreter (`dispatch_fmt_with_user_coercion`). A
/// positional collection joins its items with a space, an associative one its
/// pairs with a newline, unless `$separator` says otherwise.
// Cost: O(f + n), f = chars of the format, n = chars of the rendering; the
// coercion probe adds O(e), e = items.
pub(crate) fn fmt_native(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let default_sep = args.len() < 2;
    let (fmt, sep) = match args {
        [] => return fmt_default(target),
        [fmt] => (fmt, None),
        [fmt, sep] => (fmt, Some(sep.to_string_value())),
        _ => return None,
    };
    // A Format object is handled by the slow-path Format dispatch (arity-aware
    // batching, separators, X::Str::Sprintf::Directives::Count).
    if matches!(fmt.view(), ValueView::Instance { class_name, .. } if class_name.resolve() == "Format")
    {
        return None;
    }
    let fmt = fmt.to_string_value();
    // A format with no value-consuming directive never reads an item, so no
    // coercion can be needed: skip the coercion probe entirely.
    let has_directives = default_sep && runtime::sprintf_directive_count(&fmt) > 0;
    let join = |parts: Vec<String>, default: &str| {
        Some(Ok(Value::str(
            parts.join(sep.as_deref().unwrap_or(default)),
        )))
    };
    match target.view() {
        ValueView::Hash(items) => {
            if has_directives && items.iter().any(|(_, v)| fmt_value_needs_coercion(v)) {
                return None;
            }
            join(
                items
                    .iter()
                    .map(|(k, v)| {
                        runtime::format_sprintf_args(&fmt, &[Value::str(k.to_string()), v.clone()])
                    })
                    .collect(),
                "\n",
            )
        }
        ValueView::Bag(items, _) => {
            if has_directives
                && items
                    .iter()
                    .any(|(k, _)| fmt_value_needs_coercion(&items.typed_key(k)))
            {
                return None;
            }
            join(
                items
                    .iter()
                    .map(|(k, v)| {
                        runtime::format_sprintf_args(
                            &fmt,
                            &[items.typed_key(k), Value::from_bigint(v.clone())],
                        )
                    })
                    .collect(),
                "\n",
            )
        }
        ValueView::Set(items, _) => {
            if has_directives
                && items
                    .iter()
                    .any(|k| fmt_value_needs_coercion(&items.typed_key(k)))
            {
                return None;
            }
            join(
                items
                    .iter()
                    .map(|k| runtime::format_sprintf_args(&fmt, &[items.typed_key(k), Value::TRUE]))
                    .collect(),
                "\n",
            )
        }
        ValueView::Mix(items, _) => {
            if has_directives
                && items
                    .iter()
                    .any(|(k, _)| fmt_value_needs_coercion(&items.typed_key(k)))
            {
                return None;
            }
            join(
                items
                    .iter()
                    .map(|(k, v)| {
                        runtime::format_sprintf_args(&fmt, &[items.typed_key(k), Value::num(*v)])
                    })
                    .collect(),
                "\n",
            )
        }
        _ if pair_key_value(target).is_some() && sep.is_none() => {
            let (k, v) = pair_key_value(target)?;
            if has_directives && (fmt_value_needs_coercion(&k) || fmt_value_needs_coercion(&v)) {
                return None;
            }
            if let Err(e) = runtime::sprintf::validate_sprintf_directives(&fmt, 2) {
                return Some(Err(e));
            }
            Some(Ok(Value::str(runtime::format_sprintf_args(&fmt, &[k, v]))))
        }
        _ if fmt_joinable_target(target) => {
            // `as_list_items` bypasses itemization: `.fmt` still iterates the
            // inner elements of `$[...]` / `$(...)`.
            let items: Vec<Value> = match target.as_list_items() {
                Some(inner) => inner.to_vec(),
                None if sep.is_none() => runtime::value_to_list_for_receiver(target),
                None => runtime::value_to_list(target),
            };
            if has_directives
                && items.iter().any(|item| match pair_key_value(item) {
                    Some((k, v)) => fmt_value_needs_coercion(&k) || fmt_value_needs_coercion(&v),
                    None => fmt_value_needs_coercion(item),
                })
            {
                return None;
            }
            match items
                .iter()
                .map(|item| fmt_list_item(&fmt, item))
                .collect::<Result<Vec<_>, _>>()
            {
                Ok(parts) => join(parts, " "),
                Err(e) => Some(Err(e)),
            }
        }
        _ if sep.is_none() => {
            if has_directives && fmt_value_needs_coercion(target) {
                return None;
            }
            if let Err(e) = runtime::sprintf::validate_sprintf_directives(&fmt, 1) {
                return Some(Err(e));
            }
            Some(Ok(Value::str(runtime::format_sprintf(&fmt, Some(target)))))
        }
        // A separator on a scalar: `fmt` takes at most one argument there.
        _ => Some(Err(RuntimeError::new(
            "Too many positionals passed; expected 1 or 2 arguments but got 3",
        ))),
    }
}

/// `.fmt` with no argument: a pair formats as `key\tvalue` (an associative
/// collection one per line), a positional collection joins its items with a
/// space, anything else is its `%s`.
// Cost: O(n), n = chars of the rendering.
fn fmt_default(target: &Value) -> Option<Result<Value, RuntimeError>> {
    let rendered = match target.view() {
        ValueView::Hash(items) => items
            .iter()
            .map(|(k, v)| {
                runtime::format_sprintf_args("%s\t%s", &[Value::str(k.to_string()), v.clone()])
            })
            .collect::<Vec<_>>()
            .join("\n"),
        ValueView::Pair(..) | ValueView::ValuePair(..) => fmt_default_item(target),
        _ if fmt_joinable_target(target) => runtime::value_to_list(target)
            .into_iter()
            .map(|item| fmt_default_item(&item))
            .collect::<Vec<_>>()
            .join(" "),
        _ => runtime::format_sprintf("%s", Some(target)),
    };
    Some(Ok(Value::str(rendered)))
}
