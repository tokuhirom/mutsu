//! The plain positional element read, `@a[$i]` for a non-negative `Int` on a
//! `List`/`Array`, shared by the VM's locked fast path
//! (`Interpreter::plain_positional_element`) and the lock-free read of a
//! published snapshot (ADR-18637, `cell_snapshot`). One implementation, so the
//! two can never disagree on what a present element, a hole or a miss answers.

use super::{ArrayData, ArrayKind, Value, ValueView};

impl ArrayData {
    /// The element at `i` of this node viewed as `kind`, or `None` when the
    /// general subscript path must answer (a shape, laziness, native storage,
    /// a typed or defaulted miss, a bind token, a `Nil` slot). Reads `self`
    /// only: the caller provides whatever exclusion the node needs.
    // Cost: O(1), plus the read of a `:=`-bound element's own cell.
    pub(crate) fn plain_positional_read(&self, kind: ArrayKind, i: i64) -> Option<Value> {
        if i < 0 {
            return None;
        }
        // `Shaped` dies on an out-of-range read and defaults its unwritten
        // slots, and `Lazy` may still need reifying: both are left to the
        // general path. An itemized Array (`$[...]`, the Array a `$` scalar or
        // a `for` parameter holds) indexes exactly like the Array itself; an
        // itemized List is left to the general path.
        if !matches!(
            kind,
            ArrayKind::List | ArrayKind::Array | ArrayKind::ItemArray
        ) {
            return None;
        }
        let real_array = kind != ArrayKind::List;
        // ADR-0030 native backing changes where a read comes from. An element
        // type or an `is default(...)` only changes what a MISS answers -- a
        // hole, an index past the end, a stored `Nil` -- so such an array still
        // answers a present ordinary element here (`TYPE @!keys` of a
        // parameterized role, read in a binary search); every miss shape is
        // left to the general path.
        if self.has_native_backing() {
            return None;
        }
        let plain_misses = !self.has_type_meta() && self.default.is_none();
        let Some(elem) = self.get(i as usize) else {
            // Past the end of an untyped real Array the read is its element
            // default, `Any` (`typed_container_default`); a List's is `Nil`,
            // which the general path answers.
            return (real_array && plain_misses).then(|| Value::package(crate::symbol::wk::any()));
        };
        match elem.view() {
            // In an untyped real Array a hole is the `Any` gap marker, and
            // `resolve_array_entry` answers it with the element default --
            // `Any` again -- while a type object stored there is read back as
            // itself. A List's `Mu` hole reads as `Nil` instead.
            ValueView::Package(..) if real_array && plain_misses => Some(elem.clone()),
            // A `:=`-bound element cell decontainerizes on read
            // (`resolve_array_entry`), unless it holds something the general
            // path still has to fetch or resolve.
            ValueView::ContainerRef(_) => {
                let value = elem.deref_container();
                (!value.is_proxy_value()
                    && !value.is_container_ref()
                    && !value.is_lazy_thunk_value()
                    && !matches!(
                        value.view(),
                        ValueView::Nil | ValueView::HashEntryRef { .. }
                    ))
                .then_some(value)
            }
            // A deferred array-entry bind token resolves to the entry it
            // names, and a `Nil` slot keeps the general path's handling.
            ValueView::Package(..) | ValueView::HashEntryRef { .. } | ValueView::Nil => None,
            _ => Some(elem.clone()),
        }
    }
}
