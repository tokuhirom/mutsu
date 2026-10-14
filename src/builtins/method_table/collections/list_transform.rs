//! Zero-argument collection transformations shared by the method table and
//! the native cascade for receiver shapes the table does not cover.

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::method_table::Named;
use crate::value::{RuntimeError, Value, ValueView, const_str_eq};

macro_rules! rows {
    ($owner:literal, $flags:expr; $($name:literal => $handler:ident),* $(,)?) => {
        &[$(MethodRow {
            owner: $owner,
            name: $name,
            arity: 0,
            handler: Handler::Narrow($handler),
            flags: $flags,
            named: &[],
        }),*]
    };
}

/// `Any`'s rows answer a type object too (`Str.sort` is `(Str,)`): Rakudo's
/// `Any` candidates take `self.list`, and a type object lists as itself.
pub(super) static ANY_ROWS: &[MethodRow] = rows!["Any", RowFlags::TYPE_OBJECT_OK;
    "sort" => sort,
    "unique" => unique,
    "repeated" => repeated,
];

pub(super) static LIST_ROWS: &[MethodRow] = rows!["List", RowFlags::NONE;
    "sort" => sort,
];

/// `flat` (and `flat(:hammer)`) on each owner Rakudo declares it on: the
/// first rows to bind a named argument.
pub(super) static FLAT_ROWS: &[MethodRow] = &[
    flat_row("Any"),
    flat_row("List"),
    flat_row("Array"),
    flat_row("Seq"),
];

pub(super) const fn flat_row(owner: &'static str) -> MethodRow {
    MethodRow {
        owner,
        name: "flat",
        arity: 0,
        handler: Handler::Named(flat_named),
        // `Any.flat` answers a type object as its one element (`Str.flat`).
        flags: if const_str_eq(owner, "Any") {
            RowFlags::TYPE_OBJECT_OK
        } else {
            RowFlags::NONE
        },
        named: &["hammer"],
    }
}

/// `flat($levels, :hammer)`: Rakudo declares it on `Iterable` (and again on
/// `Array`); `Any` and `Range` declare only the form with no level, so a
/// number, a string or a `Range` has no row for it.
pub(super) static FLAT_LEVELS_ROWS: &[MethodRow] = &[
    flat_levels_row("List"),
    flat_levels_row("Array"),
    flat_levels_row("Seq"),
    flat_levels_row("Map"),
];

const fn flat_levels_row(owner: &'static str) -> MethodRow {
    MethodRow {
        owner,
        name: "flat",
        arity: 1,
        handler: Handler::Named(flat_levels),
        // `$levels` is `.Int`-coerced, so a `Str`, a `Rat` or a `Whatever`
        // binds as well as an `Int`.
        flags: RowFlags::ANY_ARGS,
        named: &["hammer"],
    }
}

/// `flat($levels, :hammer)`: flatten `$levels` levels of nested lists (every
/// level for a negative count, `Inf` or `*`); `:hammer` descends into
/// itemized arrays too. An `Array`'s own elements are items, so without
/// `:hammer` the level changes nothing. A `Hash` flattens as its pairs.
// Cost: O(t), t = leaves reached within `$levels` levels (`flatten_target`
// walks eagerly); O(n) more for a Str level, n = chars (the parse).
fn flat_levels(
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let hammer = named.get("hammer").is_some_and(Value::truthy);
    // `Array.flat($levels)` without `:hammer` never reads the level.
    if !hammer
        && matches!(
            target.view(),
            ValueView::Array(
                _,
                crate::value::ArrayKind::Array | crate::value::ArrayKind::ItemArray
            )
        )
    {
        return flat(target, &[]);
    }
    let levels = match flat_levels_of(args.first()?)? {
        Ok(levels) => levels,
        Err(err) => return Some(Err(err)),
    };
    match target.view() {
        // A list still being produced may be infinite: the eager walk below
        // would not end.
        ValueView::LazyList(_) => None,
        ValueView::Hash(_) => {
            let pairs = Value::array(crate::runtime::utils::value_to_list_for_receiver(target));
            Some(Ok(crate::builtins::methods_narg::flatten::flatten_target(
                &pairs, levels, hammer,
            )))
        }
        _ => Some(Ok(crate::builtins::methods_narg::flatten::flatten_target(
            target, levels, hammer,
        ))),
    }
}

/// The level count `$levels.Int` stands for: `None` for every level (a
/// negative count, `Inf`, `*`). A non-numeric `Str` dies as its `.Int` does;
/// a value with no plain number declines.
// Cost: O(n) for a Str, n = chars; O(1) otherwise.
fn flat_levels_of(arg: &Value) -> Option<Result<Option<usize>, RuntimeError>> {
    let count = |n: i64| Some(Ok(usize::try_from(n).ok()));
    match arg.view() {
        ValueView::Whatever => Some(Ok(None)),
        ValueView::Int(n) => count(n),
        ValueView::Bool(b) => count(i64::from(b)),
        ValueView::Num(f) if f.is_nan() => None,
        // `Inf` and `-Inf` both mean every level (a negative count does).
        ValueView::Num(f) if f.is_infinite() => Some(Ok(None)),
        ValueView::Num(f) => count(f.trunc() as i64),
        ValueView::Rat(0, 0) => None,
        ValueView::Rat(_, 0) => Some(Ok(None)),
        ValueView::Rat(n, d) => count(n / d),
        ValueView::Str(s) => match crate::builtins::method_table::numify::numify_str(&s) {
            Some(number) => flat_levels_of(&number),
            None => Some(Err(crate::builtins::methods_0arg::str_numeric_error(&s))),
        },
        _ => None,
    }
}

/// `flat` and `flat(:hammer)`. A false `:hammer` is outside the row's
/// signature (it binds the adverb only when set), so it takes the cascades.
// Cost: O(1) for `flat` itself (see [`flat`]); O(t) for `:hammer`, t = leaves
// of the receiver, which `flatten_target` walks eagerly.
fn flat_named(
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    match named.get("hammer") {
        None => flat(target, args),
        Some(hammer) if hammer.truthy() => Some(Ok(flat_hammer(target))),
        Some(_) => None,
    }
}

/// `flat(:hammer)`: flatten every level, arrays included.
// Cost: O(t), t = leaves of the receiver.
fn flat_hammer(target: &Value) -> Value {
    // A `Hash` flattens as its pairs (`%h.flat(:hammer)` is `%h.flat`).
    if let ValueView::Hash(_) = target.view() {
        let pairs = Value::array(crate::runtime::utils::value_to_list_for_receiver(target));
        return crate::builtins::methods_narg::flatten::flatten_target(&pairs, None, true);
    }
    crate::builtins::methods_narg::flatten::flatten_target(target, None, true)
}

/// Flatten a collection one level, preserving the existing lazy and shaped
/// Array cases.
// Cost: O(1) per call for lazy List/Array, LazyList and infinite Range;
// otherwise O(t), t = leaves reached through flattenable nesting.
pub(crate) fn flat(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        ValueView::Array(_, crate::value::ArrayKind::Shaped) => {
            let leaves = crate::runtime::utils::shaped_array_leaves(target);
            Some(Ok(Value::seq(leaves)))
        }
        _ if crate::builtins::methods_0arg::is_infinite_range(target) => Some(Ok(target.clone())),
        ValueView::LazyList(_) => Some(Ok(target.clone())),
        _ => {
            // De-itemize the top-level receiver first; nested itemized items
            // stay single in flat_val, matching Raku's List semantics.
            let operand = crate::builtins::deitemize_flat_operand(target);
            if let ValueView::Array(
                _,
                kind @ (crate::value::ArrayKind::Array | crate::value::ArrayKind::List),
            ) = operand.view()
            {
                let flatten_children = kind == crate::value::ArrayKind::List;
                return Some(Ok(Value::seq_list_gen(
                    crate::value::ListGen::flat(operand, flatten_children),
                    false,
                )));
            }
            let mut result = Vec::new();
            crate::builtins::flat_val(&operand, &mut result, true);
            Some(Ok(Value::seq(result)))
        }
    }
}

/// Sort a reified Array when comparison needs no user dispatch. The cascade
/// handles all other receivers and any array containing user-comparable
/// objects.
// Cost: O(e log e) comparisons and O(e) copied values; e = array elements.
pub(crate) fn sort(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        ValueView::Array(items, kind) if kind != crate::value::ArrayKind::ItemArray => {
            if crate::runtime::utils::sort_needs_dispatched_cmp(items.iter()) {
                return None;
            }
            let mut sorted = if kind == crate::value::ArrayKind::Shaped
                && items
                    .iter()
                    .any(|value| matches!(value.view(), ValueView::Array(..)))
            {
                crate::runtime::utils::shaped_array_leaves(target)
            } else {
                // Each element is read once, before the sort: a bound element
                // cell that another thread writes to mid-sort would otherwise
                // compare differently on each visit, which `sort_by` may
                // answer with a panic (ADR-0068 §20). `.sort` answers values.
                (**items)
                    .clone()
                    .into_items()
                    .into_iter()
                    .map(|v| {
                        if v.is_container_ref() {
                            v.deref_container()
                        } else {
                            v
                        }
                    })
                    .collect()
            };
            sorted.sort_by(|a, b| crate::runtime::compare_values(a, b).cmp(&0));
            Some(Ok(Value::seq(sorted)))
        }
        ValueView::Hash(map) => {
            let mut items: Vec<Value> = map
                .iter()
                .map(|(key, value)| map.typed_pair(key, value.clone()))
                .collect();
            if items
                .iter()
                .any(|item| identity_needs_user_dispatch(item, 0))
                || crate::runtime::utils::sort_needs_dispatched_cmp(items.iter())
            {
                return None;
            }
            items.sort_by(|a, b| crate::runtime::compare_values(a, b).cmp(&0));
            Some(Ok(Value::seq(items)))
        }
        // These scalar types use Any.sort's one-element List semantics. Other
        // receiver shapes (Seq, Range, Set/Bag/Mix, Uni and mixins) have
        // collection-specific behavior in the existing interpreter path.
        ValueView::Str(_)
        | ValueView::Bool(_)
        | ValueView::Int(_)
        | ValueView::BigInt(_)
        | ValueView::Num(_)
        | ValueView::Rat(..)
        | ValueView::BigRat(..)
        | ValueView::FatRat(..)
        | ValueView::Complex(..) => Some(Ok(Value::seq(vec![target.clone()]))),
        // A `Date`, `DateTime`, `Instant` or `Duration` is one item too.
        _ if super::any_collection::scalar_like(target) => {
            Some(Ok(Value::seq(vec![target.clone()])))
        }
        _ => None,
    }
}

/// Whether the pure identity index would need user method dispatch for a key.
// Cost: O(d), d = nested Pair depth, bounded at 16.
fn identity_needs_user_dispatch(value: &Value, depth: u8) -> bool {
    if depth >= 16 {
        return true;
    }
    match value.view() {
        ValueView::Instance { .. }
        | ValueView::Mixin(..)
        | ValueView::CustomTypeInstance(..)
        | ValueView::Package(..)
        | ValueView::Scalar(..)
        | ValueView::ContainerRef(..)
        | ValueView::ContainerView(..)
        | ValueView::Proxy { .. } => true,
        ValueView::Pair(_, value) => identity_needs_user_dispatch(value, depth + 1),
        ValueView::ValuePair(key, value) => {
            identity_needs_user_dispatch(key, depth + 1)
                || identity_needs_user_dispatch(value, depth + 1)
        }
        _ => false,
    }
}

/// Keep the first value of each identity class.
// Cost: O(e) average for bucketed values, O(e * u) otherwise; e = elements,
// u = distinct values of kinds that require equality scans. Rakudo: O(e) --
// see #9161.
fn unique_seq<'a>(items: impl Iterator<Item = &'a Value>) -> Value {
    let mut seen = crate::runtime::IdentityIndex::new();
    let mut result = Vec::new();
    for item in items {
        if !seen.contains(item) {
            seen.insert(item.clone());
            result.push(item.clone());
        }
    }
    Value::seq(result)
}

/// Keep each value after its first occurrence.
// Cost: O(e) average for bucketed values, O(e * u) otherwise; e = elements,
// u = distinct values of kinds that require equality scans. Rakudo: O(e) --
// see #9161.
fn repeated_seq<'a>(items: impl Iterator<Item = &'a Value>) -> Value {
    let mut seen = crate::runtime::IdentityIndex::new();
    let mut result = Vec::new();
    for item in items {
        if seen.contains(item) {
            result.push(item.clone());
        } else {
            seen.insert(item.clone());
        }
    }
    Value::seq(result)
}

/// The same zero-argument handler is used by rows and by the cascade for
/// receiver kinds without a table shape (including Seq and Slip).
// Cost: O(e) average for bucketed values, O(e * u) otherwise; e = elements,
// u = distinct values of kinds that require equality scans. Rakudo: O(e) --
// see #9161.
pub(crate) fn unique(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        ValueView::Array(items, _) => {
            if items
                .iter()
                .any(|item| identity_needs_user_dispatch(item, 0))
            {
                None
            } else {
                Some(Ok(unique_seq(items.iter())))
            }
        }
        ValueView::Seq(items) => {
            if items
                .iter()
                .any(|item| identity_needs_user_dispatch(item, 0))
            {
                None
            } else {
                Some(Ok(unique_seq(items.iter())))
            }
        }
        ValueView::Slip(items) => {
            if items
                .iter()
                .any(|item| identity_needs_user_dispatch(item, 0))
            {
                None
            } else {
                Some(Ok(unique_seq(items.iter())))
            }
        }
        ValueView::Hash(_) => {
            let items = crate::runtime::utils::value_to_list_for_receiver(target);
            if items
                .iter()
                .any(|item| identity_needs_user_dispatch(item, 0))
            {
                None
            } else {
                Some(Ok(unique_seq(items.iter())))
            }
        }
        ValueView::Pair(..) | ValueView::ValuePair(..) => {
            Some(Ok(Value::seq(vec![target.clone()])))
        }
        ValueView::Bool(_) => Some(Ok(Value::seq(vec![target.clone()]))),
        ValueView::LazyList(_) => None,
        ValueView::Instance { class_name, .. } if class_name == "Supply" => None,
        // A scalar (Int, Str, an allomorph, ...) reads as its one-element
        // list: `Any.unique` is `self.list.unique`. `dispatch_unique` does that.
        _ => None,
    }
}

/// The same zero-argument handler is used by rows and by the cascade for
/// receiver kinds without a table shape (including Seq and Slip).
// Cost: O(e) average for bucketed values, O(e * u) otherwise; e = elements,
// u = distinct values of kinds that require equality scans. Rakudo: O(e) --
// see #9161.
pub(crate) fn repeated(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        ValueView::Array(items, _) => {
            if items
                .iter()
                .any(|item| identity_needs_user_dispatch(item, 0))
            {
                None
            } else {
                Some(Ok(repeated_seq(items.iter())))
            }
        }
        ValueView::Seq(items) => {
            if items
                .iter()
                .any(|item| identity_needs_user_dispatch(item, 0))
            {
                None
            } else {
                Some(Ok(repeated_seq(items.iter())))
            }
        }
        ValueView::Slip(items) => {
            if items
                .iter()
                .any(|item| identity_needs_user_dispatch(item, 0))
            {
                None
            } else {
                Some(Ok(repeated_seq(items.iter())))
            }
        }
        ValueView::LazyList(_) => None,
        _ => Some(Ok(Value::seq(Vec::new()))),
    }
}
