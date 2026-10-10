//! `clone` of the aggregates (ADR-11276 slice 3C remainder, #12389 item 1).
//!
//! `Array`, `Hash` and `Pair` declare `clone` (an immutable `Map` is answered by `Hash`'s row). One copy
//! routine, [`container_clone`], serves the cascade's arm and the
//! `Handler::Interp` rows; the row then re-tags the declared element/key type
//! (kept in an interpreter side table for arrays), as the VM's post-hoc step
//! for the native path always did.

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::method_table::Named;
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value, ValueView};

pub(super) static ROWS: &[MethodRow] = &[
    clone_row("Array"),
    clone_row("Hash"),
    clone_row("Pair"),
];

const fn clone_row(owner: &'static str) -> MethodRow {
    MethodRow {
        owner,
        name: "clone",
        arity: 0,
        handler: Handler::Interp(clone),
        flags: RowFlags::NONE,
        named: &[],
    }
}

/// The row handler: the shared copy, then the receiver's declared container
/// type carried over to it.
// Cost: O(e), e = elements of the invocant.
fn clone(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    if !args.is_empty() {
        return None;
    }
    let copy = container_clone(target)?;
    Some(Ok(match interp.container_type_metadata(target) {
        Some(info) => interp.tag_container_metadata(copy, info),
        None => copy,
    }))
}

/// A shallow copy of an `Array`, `Hash`, `Pair` or `ValuePair`; `None` for any
/// other receiver.
// Cost: O(e), e = elements of the invocant (O(d) per shaped row, d = cells).
pub(crate) fn container_clone(target: &Value) -> Option<Value> {
    Some(match target.view() {
        // Clone the whole `ArrayData`, not just its elements, so
        // per-slot metadata (`initialized` deletion holes, `default`,
        // `shape`, element type) survives the copy — e.g.
        // `@a[3]:delete; my @b := @a.clone` keeps `@b[3]:exists` False
        // (S02-types/array.t test 108). Elements are `Value`-cloned
        // (shared handles), matching a shallow `.clone` — EXCEPT for a
        // shaped array, whose rows are its own storage: rakudo's
        // `.clone` gives independent containers per dimension
        // (rakudo#3334, S09-multidim/methods.t), and with in-place
        // element writes (container identity §3) shared rows would
        // alias `@a[1;1] = v` into the clone.
        ValueView::Array(items, kind) => {
            let mut data = (**items).clone();
            // The clone gets containers of its own: a slot promoted to a
            // shared cell (a `for ... is rw` alias, a `:=` bind) must
            // not keep aliasing the source (`my @c = @a.clone; @c[0] = 9`
            // leaves `@a` alone, as in rakudo).
            for item in data.live_mut().iter_mut() {
                if item.is_container_ref() {
                    *item = item.deref_container();
                }
            }
            if kind == crate::value::ArrayKind::Shaped || data.shape.is_some() {
                fn clone_rows(v: &Value) -> Value {
                    match v.view() {
                        ValueView::Array(rows, k) => {
                            let mut d = (**rows).clone();
                            for item in d.live_mut().iter_mut() {
                                *item = clone_rows(item);
                            }
                            Value::array_with_kind(crate::gc::Gc::new(d), k)
                        }
                        _ => v.clone(),
                    }
                }
                for item in data.live_mut().iter_mut() {
                    *item = clone_rows(item);
                }
            }
            // Itemization is a property of the CONTAINER, not of the
            // object, and `.clone` copies the object. So the clone comes
            // back de-itemized, exactly as rakudo has it:
            // `my $v = <a b c>; $v.raku` is `$("a", "b", "c")` but
            // `$v.clone.raku` is `("a", "b", "c")` — which is why
            // `my @a; @a = $v.clone` flattens where `@a = $v` does not.
            // (Crane's `Crane::In.in(container, @path) = $value.clone`
            // depends on exactly that: a List value must land in the
            // target array as elements, not as one nested list.)
            Value::array_with_kind(crate::gc::Gc::new(data), kind.decontainerize())
        }
        ValueView::Hash(map) => Value::hash_with_data(Value::hash_arc((**map).clone())),
        ValueView::Pair(key, value) => Value::pair(key.clone(), value.clone()),
        ValueView::ValuePair(key, value) => Value::value_pair(key.clone(), value.clone()),
        _ => return None,
    })
}
