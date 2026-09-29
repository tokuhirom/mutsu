//! Slice stores through a method accessor: `$obj.h{@keys} = @values`.
use super::*;
use crate::value::ValueView;

impl Interpreter {
    /// The keys of a SLICE subscript given to `__mutsu_index_assign_method_lvalue`,
    /// or `None` when the subscript names a single element.
    ///
    /// Only a non-itemized list slices. An itemized one (`$c.h{$k}` with
    /// `my $k = [1, 2]`, or a `$_` bound to an element of an array of arrays)
    /// is ONE subscript: a hash keys it by its `.Str` (`"1 2"`), exactly as the
    /// named-variable store does (`%h{$k} = v`). A `[i;j]` multi-dim subscript
    /// never reaches the accessor builtin -- the compiler lowers it to
    /// `MultiDimIndexAssignGeneric` -- so a list index here is never a
    /// coordinate tuple.
    // Cost: O(k), k = slice keys (the elements of a finite Range subscript).
    pub(super) fn method_lvalue_slice_keys(index: &Value) -> Option<Vec<Value>> {
        match index.view() {
            ValueView::Array(items, kind)
                if !kind.is_itemized() && !matches!(kind, crate::value::ArrayKind::Lazy) =>
            {
                Some(items.to_vec())
            }
            ValueView::Seq(items) => Some(items.to_vec()),
            ValueView::Slip(items) => Some(items.to_vec()),
            // A finite Range subscript (`$c.a[0..2]`) slices its elements; an
            // open-ended one (`0..*`) is left to the element path.
            ValueView::Range(_, end)
            | ValueView::RangeExcl(_, end)
            | ValueView::RangeExclStart(_, end)
            | ValueView::RangeExclBoth(_, end)
                if end != i64::MAX =>
            {
                Some(crate::runtime::utils::value_to_list(index))
            }
            _ => None,
        }
    }

    /// `$obj.method{k1, k2} = v1, v2`: a slice store through an accessor is one
    /// element store per key, zipped with the RHS list. A key past the end of
    /// the RHS stores `Nil`, which the element store turns into the container's
    /// default (`Any`, or its `is default`). The value is the List of stored
    /// values, as in rakudo (`($c.h<a b> = 1, 2).raku` is `(1, 2)`).
    ///
    /// `args` is the builtin's own argument vector; each key re-enters the
    /// single-element path with `args[index_pos]` / `args[index_pos + 1]`
    /// replaced, so typed/object hashes, `is default`, Proxy accessors and the
    /// setter write-back all behave exactly as for `$obj.method{k} = v`.
    // Cost: O(k * s), k = slice keys, s = one accessor element store.
    pub(super) fn index_assign_method_lvalue_slice(
        &mut self,
        args: &[Value],
        index_pos: usize,
        keys: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        // The RHS of a slice store is a list assignment, so even an itemized
        // Array flattens (`$c.h<p q> = $x` with `my $x = [1, 2]` stores 1 and
        // 2), matching the named-variable slice store.
        let rhs = &args[index_pos + 1];
        let values = match rhs.view() {
            ValueView::Array(items, _) => items.to_vec(),
            _ => crate::runtime::utils::value_to_list(rhs),
        };
        let mut element_args = args.to_vec();
        let mut stored = Vec::with_capacity(keys.len());
        for (i, key) in keys.into_iter().enumerate() {
            element_args[index_pos] = key;
            element_args[index_pos + 1] = values.get(i).cloned().unwrap_or(Value::NIL);
            stored.push(self.builtin_index_assign_method_lvalue(&element_args)?);
        }
        Ok(Value::array_with_kind(
            crate::gc::Gc::new(crate::value::ArrayData::new(stored)),
            crate::value::ArrayKind::List,
        ))
    }
}
