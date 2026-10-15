//! `Any`'s key/value views and the other `self.list.METHOD` methods a receiver
//! with no row of its own reaches (ADR-11276 §9.109): `keys`, `values`, `kv`,
//! `pairs`, `antipairs`, `pairup`, `invert`, `permutations`, `combinations`
//! and `cache`.
//!
//! Each handler carries the whole body of the cascade arm it replaced
//! (`methods_0arg/collection.rs`, deleted): the owners that declare their own
//! row (`List`, `Map`, `Pair`, `Capture`, the quant hashes, `Seq.cache`) answer
//! their shapes first, and these answer what is left: a scalar as its
//! one-element list, a shaped array by its leaves, a `Range`, a `Seq` or a
//! `Slip` as its elements, an enum value, a stash and `Nil`. A type object
//! declines, so the interpreter answers it (an enum type lists its values).

use super::{Handler, MethodRow, RowFlags};
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueMap, ValueView};

macro_rules! any_rows {
    ($($name:literal => $handler:path),* $(,)?) => {
        &[$(MethodRow {
            owner: "Any",
            name: $name,
            arity: 0,
            handler: Handler::Narrow($handler),
            flags: RowFlags::NONE,
            named: &[],
        }),*]
    };
}

pub(super) static ROWS: &[MethodRow] = any_rows![
    "keys" => keys,
    "values" => values,
    "kv" => kv,
    "pairs" => pairs,
    "antipairs" => antipairs,
    "invert" => invert,
    "permutations" => super::list_aggregate::permutations,
    "combinations" => super::list_aggregate::combinations,
    "cache" => cache,
];

/// `Any.pairup` declines nothing: a type object lists as nothing
/// (`Range.pairup` is `()`).
pub(super) static PAIRUP_ROWS: &[MethodRow] = &[MethodRow {
    owner: "Any",
    name: "pairup",
    arity: 0,
    handler: Handler::Narrow(pairup),
    flags: RowFlags::TYPE_OBJECT_OK,
    named: &[],
}];

/// The `(index, leaf)` pairs of a shaped array, the index a plain `Int` for a
/// one-dimensional array and the list of its coordinates otherwise.
// Cost: O(e), e = leaves.
fn shaped_indexed(target: &Value) -> Vec<(Value, Value)> {
    crate::runtime::utils::shaped_array_indexed_leaves(target)
        .into_iter()
        .map(|(idx, leaf)| {
            let key = if idx.len() == 1 {
                Value::int(idx[0])
            } else {
                Value::array(idx.into_iter().map(Value::int).collect())
            };
            (key, leaf)
        })
        .collect()
}

/// The elements a receiver with no view of its own lists as, numbered.
// Cost: O(e), e = elements.
fn indexed_elements(target: &Value) -> impl Iterator<Item = (usize, Value)> {
    crate::runtime::utils::value_to_list(target)
        .into_iter()
        .enumerate()
}

/// What every view answers before its receiver-specific part: a scalar's own
/// row, a shaped array's leaves, the owner rows of a hash, a pair, a list and
/// a quant hash. `None` falls through to the generic part.
// Cost: O(1) to decide, plus the answering handler's own cost.
fn shared_prefix(
    target: &Value,
    scalar: fn(&Value, &[Value]) -> Result<Value, RuntimeError>,
    shaped: fn(Vec<(Value, Value)>) -> Vec<Value>,
    owner: [fn(&Value, &[Value]) -> Option<Result<Value, RuntimeError>>; 4],
) -> Option<Option<Result<Value, RuntimeError>>> {
    if super::any_collection::scalar_like(target) {
        return Some(Some(scalar(target, &[])));
    }
    // `Any`'s views on an undefined invocant: Rakudo's
    // `multi method keys(Any:U:) { () }` makes `Nil.keys` the empty List.
    if target.is_nil() {
        return Some(Some(Ok(Value::array(Vec::new()))));
    }
    if crate::runtime::utils::is_shaped_array(target) {
        return Some(Some(Ok(Value::seq(shaped(shaped_indexed(target))))));
    }
    let [map, pair, list, quant] = owner;
    Some(match target.view() {
        ValueView::Hash(_) => map(target, &[]),
        ValueView::Pair(..) | ValueView::ValuePair(..) => pair(target, &[]),
        ValueView::Array(..) => list(target, &[]),
        ValueView::Set(..) | ValueView::Bag(..) | ValueView::Mix(..) => quant(target, &[]),
        // A type object (an enum type lists its values) is the interpreter's.
        ValueView::Package(_) => None,
        _ => return None,
    })
}

/// `Any.keys`: the indices of `self.list`.
// Cost: O(e), e = elements (or pairs), built eagerly; an `Array` or `List`
// answers from its own row (a lazy counting iterator).
pub(crate) fn keys(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if let Some(answer) = shared_prefix(
        target,
        super::any_collection::keys,
        |leaves| leaves.into_iter().map(|(key, _)| key).collect(),
        [map_keys, super::pair::keys, list_keys, super::quanthash::keys],
    ) {
        return answer;
    }
    Some(Ok(Value::seq(
        indexed_elements(target)
            .map(|(idx, _)| Value::int(idx as i64))
            .collect(),
    )))
}

fn map_keys(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    Some(super::map::keys(target, args))
}

fn list_keys(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    Some(super::list::keys(target, args))
}

/// `Any.values`: `self.list`.
// Cost: O(e), e = elements (or pairs), copied eagerly; O(1) for a `Seq` over a
// gather still being pulled.
pub(crate) fn values(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if let Some(answer) = shared_prefix(
        target,
        super::any_collection::values,
        |leaves| leaves.into_iter().map(|(_, leaf)| leaf).collect(),
        [
            |t, a| Some(super::map::values(t, a)),
            super::pair::values,
            |t, a| Some(super::list::values(t, a)),
            super::quanthash::values,
        ],
    ) {
        return answer;
    }
    match target.view() {
        // A lazy list is forced by the interpreter.
        ValueView::LazyList(_) => None,
        // A List over a gather still being pulled is its own `.values`
        // (ADR-11678): it reifies from the coroutine as it is read.
        ValueView::Seq(body) if body.is_gather_source() => Some(Ok(target.clone())),
        _ => Some(Ok(Value::seq(crate::runtime::utils::value_to_list(target)))),
    }
}

/// `Any.kv`: index, element, index, element, ...
// Cost: O(e), e = elements (or pairs), built eagerly.
pub(crate) fn kv(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if let Some(answer) = shared_prefix(
        target,
        super::any_collection::kv,
        |leaves| {
            leaves
                .into_iter()
                .flat_map(|(key, leaf)| [key, leaf])
                .collect()
        },
        [
            |t, a| Some(super::map::kv(t, a)),
            super::pair::kv,
            |t, a| Some(super::list::kv(t, a)),
            super::quanthash::kv,
        ],
    ) {
        return answer;
    }
    match target.view() {
        // A stash entry is the symbol's own container (a root `our $x`'s
        // shared cell), so `for OUR::.kv -> \k, \v { v = Nil }` writes the
        // variable, as in Rakudo (P5reset).
        ValueView::Instance { class_name, .. }
            if crate::value::types::is_stash_class_name(&class_name.resolve()) =>
        {
            let kv = crate::runtime::utils::stash_symbol_entries(target)
                .into_iter()
                .flat_map(|(key, value)| [key, value])
                .collect();
            Some(Ok(Value::seq(kv)))
        }
        ValueView::Enum { key, value, .. } => Some(Ok(Value::seq(vec![
            Value::str(key.resolve()),
            value.to_value(),
        ]))),
        _ => Some(Ok(Value::seq(
            indexed_elements(target)
                .flat_map(|(idx, value)| [Value::int(idx as i64), value])
                .collect(),
        ))),
    }
}

/// `Any.pairs`: index => element.
// Cost: O(e), e = elements (or pairs), one Pair allocated per element.
pub(crate) fn pairs(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if let Some(answer) = shared_prefix(
        target,
        super::any_collection::pairs,
        |leaves| {
            leaves
                .into_iter()
                .map(|(key, leaf)| Value::value_pair(key, leaf))
                .collect()
        },
        [
            |t, a| Some(super::map::pairs(t, a)),
            super::pair::pairs,
            |t, a| Some(super::list::pairs(t, a)),
            super::quanthash::pairs,
        ],
    ) {
        return answer;
    }
    match target.view() {
        // A stash lists as its symbol pairs.
        ValueView::Instance { class_name, .. }
            if crate::value::types::is_stash_class_name(&class_name.resolve()) =>
        {
            Some(Ok(Value::seq(crate::runtime::utils::value_to_list(target))))
        }
        _ => Some(Ok(Value::seq(
            indexed_elements(target)
                .map(|(idx, value)| Value::value_pair(Value::int(idx as i64), value))
                .collect(),
        ))),
    }
}

/// `Any.antipairs`: element => index.
// Cost: O(e), e = elements (or pairs), one Pair allocated per element.
pub(crate) fn antipairs(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if let Some(answer) = shared_prefix(
        target,
        super::any_collection::antipairs,
        |leaves| {
            leaves
                .into_iter()
                .map(|(key, leaf)| Value::value_pair(leaf, key))
                .collect()
        },
        [
            |t, a| Some(super::map::antipairs(t, a)),
            super::pair::antipairs,
            |t, a| Some(super::list::antipairs(t, a)),
            super::quanthash::antipairs,
        ],
    ) {
        return answer;
    }
    Some(Ok(Value::seq(
        indexed_elements(target)
            .map(|(idx, value)| crate::value::list_gen::positional_antipair(&value, idx))
            .collect(),
    )))
}

/// `Any.pairup`: a `Pair` element is kept, any other element takes the next
/// one as its value; an odd element left over is `X::Pairup::OddNumber`.
// Cost: O(e), e = elements.
fn pairup(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    // A bare type object lists as nothing, so `Range.pairup` is `()`.
    if let ValueView::Package(_) = target.view() {
        return Some(Ok(Value::seq(Vec::new())));
    }
    // ADR-0040 slices 1-2: `target` is `.pairup`'s own receiver, decomposed
    // into its elements whatever itemization it carries as an element of some
    // other container (`[[2,3],[4,[5,6]]]».pairup` hands each itemized
    // `$[2, 3]` to `.pairup`, which must still see two elements).
    let items = crate::runtime::utils::value_to_list_for_receiver(target);
    let mut pairs = Vec::with_capacity(items.len() / 2 + 1);
    let mut i = 0;
    while i < items.len() {
        if matches!(
            items[i].view(),
            ValueView::Pair(..) | ValueView::ValuePair(..)
        ) {
            pairs.push(items[i].clone());
            i += 1;
        } else if i + 1 < items.len() {
            pairs.push(Value::value_pair(items[i].clone(), items[i + 1].clone()));
            i += 2;
        } else {
            let msg = "Odd number of elements found for .pairup()";
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("message".to_string(), Value::str(msg.to_string()));
            let ex = Value::make_instance(Symbol::intern("X::Pairup::OddNumber"), attrs);
            let mut err = RuntimeError::new(format!("X::Pairup::OddNumber: {msg}"));
            err.exception = Some(Box::new(ex));
            return Some(Err(err));
        }
    }
    Some(Ok(Value::seq(pairs)))
}

/// `Any.invert`: `self.map: *.antipair`, so every element must be a `Pair`
/// (`X::TypeCheck` naming the first that is not).
// Cost: O(e + v), e = pairs of the invocant, v = values produced (a Positional
// value fans out to one pair per element).
pub(crate) fn invert(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if target.is_nil() {
        return Some(Ok(Value::array(Vec::new())));
    }
    if let Some(result) = super::list::invert(target, args) {
        return Some(result);
    }
    let elements = match target.view() {
        // An itemized `$`-held list reports its offending element, not the
        // whole `Array`.
        ValueView::Array(items, _) => items.iter().cloned().collect::<Vec<_>>(),
        ValueView::Seq(..) | ValueView::Slip(..) => crate::runtime::utils::value_to_list(target),
        _ => return None,
    };
    let got = elements
        .into_iter()
        .find(|item| !matches!(item.view(), ValueView::Pair(..) | ValueView::ValuePair(..)))
        .unwrap_or(Value::NIL);
    let got_type = crate::value::types::what_type_name(&got);
    let mut attrs = ValueMap::default();
    attrs.insert("operation".to_string(), Value::str_from("invert"));
    attrs.insert(
        "expected".to_string(),
        Value::package(Symbol::intern("Pair")),
    );
    attrs.insert("got".to_string(), got);
    attrs.insert(
        "message".to_string(),
        Value::str(format!(
            "Type check failed in invert; expected Pair but got {got_type}",
        )),
    );
    Some(Err(RuntimeError::typed("X::TypeCheck", attrs)))
}

/// `Any.cache`: the receiver's elements as a `List`, without forcing a lazy
/// one.
// Cost: O(1) on an Array, a List or a Seq (a reified or unpulled Seq is
// handed back as a List view over its own body); O(e) on any other list-like,
// e = elements (copied into a fresh List).
fn cache(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    // A genuinely lazy list (an infinite sequence, a lazy pipe, a cat-handle
    // pull) stays lazy under `.cache`: Rakudo's `.cache` reifies on demand.
    // Forcing here would defeat `(my $l = $cat.lines).cache`, whose later
    // `$l[2,3]` must reflect mid-iteration attribute changes.
    if let ValueView::LazyList(ll) = target.view()
        && let Some(result) = ll.cache_lazy_view()
    {
        return Some(Ok(result));
    }
    // An already reified Positional caches as itself (`@a.cache` is `@a`, a
    // List's is `self`). A method result is decontainerized, so an itemized
    // invocant (`$node.cache`) returns the plain array: `for $node.cache`
    // iterates elements, not the item (zef's `Zef::Config::plugin-lookup`).
    if let ValueView::Array(items, kind) = target.view()
        && (kind.is_real_array()
            || matches!(
                kind,
                crate::value::ArrayKind::List | crate::value::ArrayKind::ItemList
            ))
    {
        return Some(Ok(Value::array_with_kind(items.clone(), kind.decontainerize())));
    }
    // ADR-0034: a `Seq` whose source has not been pulled is not forced; the
    // next touch reifies and keeps (`Seq.cache`'s row).
    if matches!(target.view(), ValueView::Seq(_)) {
        return super::seq::cache(target, args);
    }
    let items = target
        .as_list_items()
        .map(|items| items.to_vec())
        .unwrap_or_else(|| crate::runtime::value_to_list(target));
    Some(Ok(Value::array(items)))
}
