//! `Any`'s list coercions: `List`, `list`, `Array`, `Slip` and `Supply`
//! (ADR-11276 §9.110). `Seq` stays the interpreter's and the VM's shared
//! `seq_coerce::to_seq_structural`, which hands out an array's own element
//! containers.
//!
//! Each handler carries the body of the cascade arm it replaced
//! (`methods_0arg/coercion.rs`): the owners that declare their own row
//! (`List`, `Seq`, `Range.list`, `Map`, the quant hashes, `Blob.list`,
//! `Supply.list`) answer their shapes first, and these answer the rest: a
//! scalar or a type object as the one-element list holding it, a `Range` as
//! its elements, a buffer as its elements, a `Slip`, a lazy list (kept lazy
//! where Rakudo keeps it so), a stash. A `Capture` is `self.list`: its
//! positionals.

use super::{Handler, MethodRow, RowFlags};
use crate::value::{RuntimeError, Value, ValueView};

/// A `Handler::Narrow` function.
type NarrowFn = fn(&Value, &[Value]) -> Option<Result<Value, RuntimeError>>;

const fn any_row(name: &'static str, handler: NarrowFn, flags: RowFlags) -> MethodRow {
    MethodRow {
        owner: "Any",
        name,
        arity: 0,
        handler: Handler::Narrow(handler),
        // `Int.List` is `((Int))`: a type object lists as itself.
        flags: RowFlags::TYPE_OBJECT_OK.or(flags),
        named: &[],
    }
}

pub(super) static ROWS: &[MethodRow] = &[
    any_row("List", list_type, RowFlags::NONE),
    any_row("list", list, RowFlags::NONE),
    any_row("Array", array, RowFlags::NONE),
    any_row("Slip", slip, RowFlags::NONE),
    // A new `Supply` has an identity of its own each time (`RANDOM`).
    any_row("Supply", supply, RowFlags::RANDOM),
];

/// `Mu.Capture`: an object's public attributes as named arguments, a list's
/// elements as positionals (`methods_0arg::coercion::value_to_capture`). The
/// scalars without attributes (`Int`, `Num`, `Str`, `Version`) and the
/// collections declare their own.
pub(super) static MU_CAPTURE_ROWS: &[MethodRow] = &[MethodRow {
    owner: "Mu",
    name: "Capture",
    arity: 0,
    handler: Handler::Pure(mu_capture),
    flags: RowFlags::NONE,
    named: &[],
}];

// Cost: O(a), a = attributes or elements of the receiver.
fn mu_capture(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    // `Any` has no attributes to read: `Any.Capture` is `\()`.
    // TODO: a type object of a class with attributes is Rakudo's "Cannot look
    // up attributes in a Foo type object"; it answers the type in a capture.
    if let ValueView::Package(name) = target.view()
        && name == "Any"
    {
        return Ok(Value::capture(
            Vec::new(),
            crate::value::ValueMap::default(),
        ));
    }
    crate::builtins::methods_0arg::coercion::value_to_capture(target)
}

/// A `Capture`'s positionals, which `self.list` reads.
// Cost: O(p), p = positionals.
fn capture_positionals(target: &Value) -> Option<Value> {
    match target.view() {
        ValueView::Capture { positional, .. } => Some(Value::array(positional.to_vec())),
        _ => None,
    }
}

/// `Any.list`.
// Cost: as [`listify`].
fn list(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    listify(target, false)
}

/// `Any.Array`.
// Cost: as [`listify`].
fn array(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if let Some(list) = capture_positionals(target) {
        return listify(&list, true);
    }
    listify(target, true)
}

/// `Any.Slip`: `self.list.Slip`.
pub(crate) fn slip(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if let Some(list) = capture_positionals(target) {
        return slip(&list, &[]);
    }
    // Cost: O(e), e = elements (O(1) for a Slip, which is returned as is).
    // A genuinely lazy list (or an unbounded Range) slips as a lazy tail:
    // the Slip carries the list whole, as `|EXPR` does, and reads reify
    // from it on demand (#13117).
    if let Some(tail) = crate::builtins::lazy_slip::lazy_slip_tail(target) {
        return Some(Ok(tail));
    }
    match target.view() {
        // The `Seq.Slip` row's implementation (`method_table::seq`).
        ValueView::Seq(_) => super::seq::slip(target, &[]),
        // The `Slip` rows' implementation (`method_table::positional`).
        ValueView::Array(..) => super::positional::slip(target, &[]),
        // `.Slip` on a Slip is the identity, but it hands out the VALUE
        // rather than the container: `my $x = slip(5, 6); $x.Slip.raku` is
        // `slip(5, 6)`, so the `$` itemization is dropped.
        ValueView::Slip(_) => Some(Ok(target.clone().with_slip_itemized(false))),
        ValueView::LazyList(ll) => {
            if ll.scan_spec.is_some() {
                let items = crate::builtins::lazy_scan::force_scan_to(&ll, 200_000);
                Some(Ok(Value::slip_arc(std::sync::Arc::new(items))))
            } else {
                let items = ll.cache.lock().unwrap().clone().unwrap_or_default();
                Some(Ok(Value::slip_arc(std::sync::Arc::new(items))))
            }
        }
        ValueView::Range(..)
        | ValueView::RangeExcl(..)
        | ValueView::RangeExclStart(..)
        | ValueView::RangeExclBoth(..)
        | ValueView::GenericRange { .. }
        | ValueView::Set(..)
        | ValueView::Bag(..)
        | ValueView::Mix(..) => Some(Ok(Value::slip(crate::runtime::utils::value_to_list(
            target,
        )))),
        // `Any.Slip` is `self.list.Slip`, so an associative slips its
        // pairs (`{:a(1)}.Slip` is `slip(:a(1),)`). A method call sees
        // through the `$` item container, so an itemized hash does too.
        ValueView::Hash(items) => Some(Ok(Value::slip(
            items
                .iter()
                .map(|(k, v)| items.typed_pair(k, v.clone()))
                .collect(),
        ))),
        _ => Some(Ok(Value::slip(vec![target.clone()]))),
    }
}

/// `Any.List`: `self.list.List`.
pub(crate) fn list_type(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if let Some(list) = capture_positionals(target) {
        return Some(Ok(list));
    }
    match target.view() {
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if crate::runtime::utils::is_native_elems_class(&class_name.resolve()) => {
            if let Some(list) = crate::value::value_buf::buf_elems_as_array(
                &attributes.as_map(),
                crate::value::ArrayKind::List,
            ) {
                Some(Ok(list))
            } else {
                Some(Ok(Value::array_with_kind(
                    crate::gc::Gc::new(crate::value::ArrayData::new(Vec::new())),
                    crate::value::ArrayKind::List,
                )))
            }
        }
        // An infinite Range's `.List` is a genuinely lazy List (Rakudo:
        // `(1..Inf).List.^name` is `List`, `.gist` is `(...)`, `.is-lazy`
        // is `True`) — not the Range itself: the range's
        // `unbounded_range::lazy_list`, whatever its element type.
        // Cost: O(1) for the infinite arm (builds a one-element seed);
        // O(e), e = elements, for the finite arm.
        ValueView::Range(a, b) => {
            if b == i64::MAX {
                Some(Ok(
                    crate::builtins::methods_0arg::coercion::unbounded_range_list(target)?,
                ))
            } else if a == i64::MIN {
                Some(Ok(target.clone()))
            } else {
                Some(Ok(Value::array((a..=b).map(Value::int).collect())))
            }
        }
        // Cost: O(1) for the infinite arm, O(e) for the finite arm (as above).
        ValueView::RangeExcl(a, b) => {
            if b == i64::MAX {
                Some(Ok(
                    crate::builtins::methods_0arg::coercion::unbounded_range_list(target)?,
                ))
            } else if a == i64::MIN {
                Some(Ok(target.clone()))
            } else {
                Some(Ok(Value::array((a..b).map(Value::int).collect())))
            }
        }
        // The `Seq.List` row's implementation (`method_table::seq`).
        ValueView::Seq(_) => super::seq::list_type(target, &[]),
        // `.List` on a Slip flattens its elements into a List (`slip(1,2,3).List`
        // is `(1 2 3)`, not `((1 2 3))`) — without this arm a Slip falls through
        // to the scalar catch-all and is wrapped as a single element.
        ValueView::Slip(items) => Some(Ok(Value::array(items.to_vec()))),
        // A shaped array falls through to the slow path, which flattens all
        // dimensions and replaces Nil slots with the type-default.
        ValueView::Array(..) if crate::runtime::utils::is_shaped_array(target) => None,
        // `.List` on a List is identity (Rakudo: `method List { self }`): hand
        // back the same immutable storage, only dropping the invocant's own
        // itemization. A List can only hold a hole once an element-wise
        // store recorded `initialized`, so one without that set has none to
        // materialize as `Nil`.
        // Cost: O(1).
        ValueView::Array(items, kind)
            if matches!(
                kind,
                crate::value::ArrayKind::List | crate::value::ArrayKind::ItemList
            ) && items.initialized.is_none() =>
        {
            Some(Ok(Value::array_with_kind(
                items.clone(),
                crate::value::ArrayKind::List,
            )))
        }
        // Cost: O(e), e = elements (a fresh, decontainerized copy).
        ValueView::Array(items, kind) => {
            // `.List` materializes array holes as literal `Nil` — even when
            // the array has an `is default(...)` value (Rakudo semantics:
            // only `.Slip` substitutes the default).
            //
            // ADR-0040 slice 2: `.List` on a REAL `Array` hands out each
            // element's *value*, not its container, so it decontainerizes
            // (measured: `@c.List[0].VAR.^name` is `Array` while
            // `@c.list[0].VAR.^name` is `Scalar`, and `@c.List[0].raku` is
            // bare `[1, 2]`). `.List` on a `List` is identity, so a
            // hand-itemized element of a List literal survives
            // (`($[1,2],).List[0].raku` is `$[1, 2]`).
            let deitemize = kind.is_real_array();
            let vec: Vec<Value> = items
                .iter()
                .enumerate()
                .map(|(i, v)| {
                    if items.hole_at(i) {
                        Value::NIL
                    } else if deitemize {
                        v.clone().deitemize_element()
                    } else {
                        v.clone()
                    }
                })
                .collect();
            Some(Ok(Value::array(vec)))
        }
        ValueView::GenericRange {
            start,
            end,
            excl_start,
            excl_end,
        } => {
            if let Some(list) =
                crate::builtins::methods_0arg::coercion::unbounded_range_list(target)
            {
                return Some(Ok(list));
            }
            // String range: expand using codepoint succession
            if let (ValueView::Str(s), ValueView::Str(e)) = (start.view(), end.view()) {
                if s.chars().count() == 1 && e.chars().count() == 1 {
                    let sc = s.chars().next().unwrap() as u32;
                    let ec = e.chars().next().unwrap() as u32;
                    let start_cp = if excl_start { sc + 1 } else { sc };
                    let items: Vec<Value> = if sc <= ec {
                        let end_cp = if excl_end { ec } else { ec + 1 };
                        (start_cp..end_cp)
                            .filter_map(char::from_u32)
                            .map(|c| Value::str(c.to_string()))
                            .collect()
                    } else {
                        let end_cp = if excl_end { ec } else { ec.saturating_sub(1) };
                        (end_cp + 1..=start_cp)
                            .rev()
                            .filter_map(char::from_u32)
                            .map(|c| Value::str(c.to_string()))
                            .collect()
                    };
                    Some(Ok(Value::array(items)))
                } else {
                    Some(Ok(Value::array(vec![target.clone()])))
                }
            } else {
                // Numeric generic range
                let items = crate::runtime::utils::value_to_list(target);
                Some(Ok(Value::array(items)))
            }
        }
        // Cost: O(1) for the infinite arm, O(e) for the finite arm (as above).
        ValueView::RangeExclStart(a, b) => {
            if b == i64::MAX {
                Some(Ok(
                    crate::builtins::methods_0arg::coercion::unbounded_range_list(target)?,
                ))
            } else {
                Some(Ok(Value::array((a + 1..=b).map(Value::int).collect())))
            }
        }
        // Cost: O(1) for the infinite arm, O(e) for the finite arm (as above).
        ValueView::RangeExclBoth(a, b) => {
            if b == i64::MAX {
                Some(Ok(
                    crate::builtins::methods_0arg::coercion::unbounded_range_list(target)?,
                ))
            } else {
                Some(Ok(Value::array((a + 1..b).map(Value::int).collect())))
            }
        }
        // `.List` on an explicitly `.lazy`-marked list keeps it lazy (raku:
        // `(1,2,3).lazy.List.is-lazy` is True), so a later `.fmt`/`.sort`/etc.
        // still throws `X::Cannot::Lazy` instead of silently materializing.
        ValueView::LazyList(ll)
            if ll
                .env
                .get("__mutsu_preserve_lazy_on_array_assign")
                .is_some() =>
        {
            Some(Ok(Value::lazy_list(crate::gc::Gc::new(
                ll.with_list_context(),
            ))))
        }
        ValueView::LazyList(_) => None, // fall through to runtime to force
        // A Channel drains to its items in the runtime (blocking receives).
        ValueView::Channel(_) => None,
        // `.List` on a Hash is its pairs, exactly like `.list`/`.Array`
        // (`%h.List` is `(:a(1),)`, not `({:a(1)},)`). Without this arm the
        // Hash fell through to the scalar catch-all and became a one-element
        // list holding the whole Hash — which is how `Cro::HTTP::Client`'s
        // `self!set-headers($request, $value.List)` saw a Hash where a Pair
        // was required and rejected every `headers => %h`.
        ValueView::Hash(_) => super::map::list(target, &[]),
        ValueView::Set(..) | ValueView::Bag(..) | ValueView::Mix(..) => {
            super::quanthash::list(target, &[])
        }
        _ => Some(Ok(Value::array(vec![target.clone()]))),
    }
}

/// `Any.list` and `Any.Array`: the receiver's elements as a `List`, or as a
/// fresh real `Array`.
fn listify(target: &Value, want_array: bool) -> Option<Result<Value, RuntimeError>> {
    // `.Array` yields a real `@`-sigiled Array; `.list` yields a List.
    // ADR-0040 slice 2: `.Array` builds a REAL Array, whose elements
    // are `Scalar` containers, so aggregates itemize on the way in
    // (`((1,2),(3,4)).Array[0].raku` is `$(1, 2)`). `.list` builds a
    // List, whose elements are not containers, so it must not.
    let wrap = |items: Vec<Value>| {
        if want_array {
            crate::runtime::utils::itemize_real_array_elements(Value::real_array(items))
        } else {
            Value::array(items)
        }
    };
    match target.view() {
        // A Range's elements: the `Range.list` row's implementation
        // (`method_table::range`).
        _ if target.is_range() => super::range::listify(target, want_array),
        // The `Supply.list` row's implementation (`method_table::supply`).
        ValueView::Instance { class_name, .. } if class_name == "Supply" => {
            super::super::supply::listify(target, want_array)
        }
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if crate::runtime::utils::is_native_elems_class(&class_name.resolve()) => Some(Ok(wrap(
            crate::value::value_buf::buf_elems_or_empty(&attributes),
        ))),
        ValueView::Instance { class_name, .. }
            if crate::value::types::is_stash_class_name(&class_name.resolve()) =>
        {
            Some(Ok(wrap(crate::runtime::utils::value_to_list(target))))
        }
        // A shaped array falls through to the slow path (flatten + Nil
        // → type-default). Non-shaped arrays keep the fast path.
        ValueView::Array(..) if crate::runtime::utils::is_shaped_array(target) => None,
        // Cost: O(1) for `.list` on a plain real Array (returns the invocant);
        // O(e), e = elements, for `.Array` (always a fresh Array) and for a
        // `.list` that must strip an itemization.
        ValueView::Array(items, _) if want_array => {
            // ADR-0040 slice 2: `.Array` builds a NEW real Array, which
            // is not itself an element of anything -- so an itemized
            // receiver's own itemization is dropped
            // (`@a[0].Array.raku` is `[1, 2]`, not `$[1, 2]`), exactly
            // as `.list` already drops it below. `wrap` re-itemizes the
            // new array's own ELEMENTS. It is new even on a plain real
            // Array receiver: raku's `@a.Array =:= @a` is False, so
            // `@a.Array.push(3)` / `$p.Array[1] = 8` must not reach
            // `@a` (#9208). Like raku, the copy is a plain Array (no
            // element type, no `is default`).
            Some(Ok(wrap(items.to_vec())))
        }
        // `.list` of a plain array is the `List.list` row's implementation
        // (`method_table::positional`): it strips an itemization and makes a
        // native array's `List`.
        ValueView::Array(..) if !want_array && super::positional::list(target, &[]).is_some() => {
            super::positional::list(target, &[])
        }
        ValueView::Array(items, kind) => {
            if want_array && (!kind.is_real_array() || kind.is_itemized()) {
                Some(Ok(wrap(items.to_vec())))
            } else if !want_array && kind.is_itemized() {
                // .list on an itemized array/list strips the itemization,
                // returning the contents as a plain List (de-itemized).
                Some(Ok(Value::array_with_kind(
                    items.clone(),
                    kind.decontainerize(),
                )))
            } else {
                Some(Ok(target.clone()))
            }
        }
        // The `Seq.list`/`Seq.Array` rows' implementation (`method_table::seq`).
        ValueView::Seq(_) => super::seq::listify(target, want_array),
        ValueView::Slip(items) => Some(Ok(wrap(items.to_vec()))),
        // A genuinely-lazy list stays lazy through `.Array`/`.list`:
        // tag it with the target context so `.WHAT` reports `Array`/`List`
        // without materializing the (possibly infinite) generator. A
        // live (not-yet-pulled) gather coroutine is equally deferred —
        // `(loop { ... }).list[2]` must pull three iterations, not
        // force the (possibly infinite) loop here.
        ValueView::LazyList(ll)
            if ll.is_genuinely_lazy()
                || (ll.coroutine.is_some() && ll.cache.lock().unwrap().is_none()) =>
        {
            if want_array {
                Some(Ok(Value::lazy_list(crate::gc::Gc::new(
                    ll.with_array_context(),
                ))))
            } else {
                Some(Ok(Value::lazy_list(crate::gc::Gc::new(
                    ll.with_list_context(),
                ))))
            }
        }
        ValueView::Channel(_) => None, // fall through to runtime for drain
        // The `Any.list` row's reading of the rest.
        _ => Some(Ok(wrap(super::any_list_hash::list_of(target)))),
    }
}

/// `Any.Supply`: a `Supply` of the receiver's elements.
pub(crate) fn supply(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if let Some(list) = capture_positionals(target) {
        return supply(&list, &[]);
    }
    // A hash or a quant hash supplies its pairs (`self.list.Supply`).
    if let Some(pairs) = crate::runtime::utils::hashlike_receiver_as_pairs_list(target, "Supply") {
        return supply(&pairs, &[]);
    }
    if let ValueView::Instance { class_name, .. } = target.view()
        && (class_name == "Supplier" || class_name == "Supplier::Preserving")
    {
        // Supplier.Supply has runtime behavior (live stream), not generic coercion.
        return None;
    }
    // .Supply on an existing Supply is a noop — return self
    if let ValueView::Instance { class_name, .. } = target.view()
        && class_name == "Supply"
    {
        return Some(Ok(target.clone()));
    }
    // The `List.Supply` row's implementation
    // (`method_table::collections::any_misc`).
    super::any_misc::supply_of(target).map(Ok)
}
