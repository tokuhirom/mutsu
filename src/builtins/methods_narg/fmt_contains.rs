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

/// Whether `item`, met as an element of a `List.fmt`, is itself `.fmt`-ed
/// (Rakudo recurses into every `List` element, itemized or not — an `Array`
/// and a `Slip` too — but formats a `Seq` or a `Range` as one value).
// Cost: O(1).
pub(crate) fn is_fmt_nested_list(item: &Value) -> bool {
    matches!(item.view(), ValueView::Array(..) | ValueView::Slip(..))
}

/// Whether `item`, a non-`List` `Iterable` (a `Seq`, a `Range` or an
/// associative), met as an element of a `List.fmt`, is itself `.fmt`-ed with
/// the format (and its own default separator) instead of being one `sprintf`
/// argument. Rakudo does so under any one-directive format but a bare `%s`
/// (no width or precision), which reads the item's `.Str`:
/// `((1,2).Seq,).fmt("<%x>")` is `"<1> <2>"`, `.fmt("<%s>")` is `"<1 2>"`.
// Cost: O(f), f = chars of the format.
pub(crate) fn fmt_item_iterates(fmt: &str, item: &Value) -> bool {
    let iterable = matches!(
        item.view(),
        ValueView::Seq(..)
            | ValueView::Range(..)
            | ValueView::RangeExcl(..)
            | ValueView::RangeExclStart(..)
            | ValueView::RangeExclBoth(..)
            | ValueView::GenericRange { .. }
            | ValueView::Hash(..)
            | ValueView::Bag(..)
            | ValueView::Set(..)
            | ValueView::Mix(..)
    );
    iterable && runtime::sprintf_directive_count(fmt) == 1 && !single_directive_is_bare_str(fmt)
}

/// Whether the one directive of `fmt` is `%s` with neither width nor
/// precision (flags alone, as in `%-s`, still read `.Str` whole).
// Cost: O(f), f = chars of the format.
fn single_directive_is_bare_str(fmt: &str) -> bool {
    let mut chars = fmt.chars().peekable();
    while let Some(c) = chars.next() {
        if c != '%' {
            continue;
        }
        if chars.peek() == Some(&'%') {
            chars.next();
            continue;
        }
        let mut spec = String::new();
        for d in chars.by_ref() {
            if d.is_ascii_alphabetic() {
                return d == 's'
                    && !spec
                        .chars()
                        .any(|x| x.is_ascii_digit() || x == '.' || x == '*');
            }
            spec.push(d);
        }
        return false;
    }
    false
}

/// Extract key and value from a Pair or ValuePair.
pub(crate) fn pair_key_value(val: &Value) -> Option<(Value, Value)> {
    match val.view() {
        ValueView::Pair(k, v) => Some((Value::str(k.to_string()), v.clone())),
        ValueView::ValuePair(k, v) => Some((k.clone(), v.clone())),
        _ => None,
    }
}

/// How one item of a `List.fmt` meets the format, as Rakudo does: a Pair item
/// under a format of other than one directive is `Pair.fmt` (key and value, the
/// typed `X::Str::Sprintf::Directives::Count` on a mismatch); every other item,
/// and a Pair under a one-directive format, is one `sprintf` argument and a
/// count other than one raises a plain `X::AdHoc` with the count message.
/// Returns the key and value to format when the item is split.
// Cost: O(f), f = chars of the format.
pub(crate) fn list_item_pair_split(
    fmt: &str,
    item: &Value,
) -> Result<Option<(Value, Value)>, RuntimeError> {
    if let Some(kv) = pair_key_value(item)
        && runtime::sprintf_directive_count(fmt) != 1
    {
        runtime::sprintf::validate_sprintf_directives(fmt, 2)?;
        return Ok(Some(kv));
    }
    validate_one_fmt_argument(fmt)?;
    Ok(None)
}

/// The one-argument arity check of `fmt` on a single value (a `List` item or a
/// scalar): Rakudo's `.fmt` dies with a plain `X::AdHoc` and this short,
/// unwrapped message where `sprintf` raises the typed
/// `X::Str::Sprintf::Directives::Count`.
// Cost: O(f), f = chars of the format.
pub(crate) fn validate_one_fmt_argument(fmt: &str) -> Result<(), RuntimeError> {
    runtime::sprintf::validate_sprintf_directives(fmt, 1).map_err(|e| {
        let is_count = e.exception.as_ref().is_some_and(|x| {
            matches!(x.view(), ValueView::Instance { class_name, .. }
                if class_name.resolve() == "X::Str::Sprintf::Directives::Count")
        });
        if is_count {
            let first = e.message.lines().next().unwrap_or_default();
            RuntimeError::new(format!("{first} supplied"))
        } else {
            e
        }
    })
}

/// Format one item of a `List.fmt` (see [`list_item_pair_split`]).
// Cost: O(f + n), f = chars of the format, n = chars of the rendered value.
pub(crate) fn fmt_list_item(fmt: &str, item: &Value) -> Result<String, RuntimeError> {
    Ok(match list_item_pair_split(fmt, item)? {
        Some((k, v)) => runtime::format_sprintf_args(fmt, &[k, v]),
        None => runtime::format_sprintf(fmt, Some(item)),
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
                        runtime::format_sprintf_args(&fmt, &[items.typed_key(k), v.to_value()])
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
            let mut parts = Vec::with_capacity(items.len());
            for item in &items {
                // A `List` item is itself `.fmt`-ed with the same format and
                // separator, as Rakudo's `List.fmt` does; another `Iterable`
                // under a non-bare-`%s` format with its own default separator.
                let nested = if is_fmt_nested_list(item) {
                    Some(args)
                } else if fmt_item_iterates(&fmt, item) {
                    Some(&args[..1])
                } else {
                    None
                };
                let rendered = if let Some(nested_args) = nested {
                    match fmt_native(item, nested_args)? {
                        Ok(v) => v.to_string_value(),
                        Err(e) => return Some(Err(e)),
                    }
                } else {
                    match fmt_list_item(&fmt, item) {
                        Ok(s) => s,
                        Err(e) => return Some(Err(e)),
                    }
                };
                parts.push(rendered);
            }
            join(parts, " ")
        }
        _ if sep.is_none() => {
            if has_directives && fmt_value_needs_coercion(target) {
                return None;
            }
            if let Err(e) = validate_one_fmt_argument(&fmt) {
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
