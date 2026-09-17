# Shaped/typed array slice assignment no longer over-pulls a lazy RHS

`subset UInt8 of UInt where * < 256; my UInt8 @bytes[4]; @bytes[^4] = (loop { 0 }); say @bytes;`
used to hang. `raku` prints `[0 0 0 0]` immediately: `(loop { 0 })` used as an expression lowers
to `gather { loop { take do { 0 } } }`, a lazy `Seq`, pulled only as far as the consumer needs.

## Root cause

The positional slice-assignment path (`exec_index_assign_expr_named_op_inner`,
`src/vm/vm_var_assign_index_named.rs`) called `assignment_rhs_values(&val)` to materialize the
RHS into a `Vec` *before* zipping it against the LHS key list (`keys`). That helper already
bounded a live `gather`/`loop`-as-expression coroutine so it wouldn't literally never terminate —
but only to a blanket 100,000-element cap (`MAX_LAZY_RANGE_PREFIX`), unrelated to how many values
the LHS could ever consume (`slice_rhs_value` never reads past `keys.len()`).

For a plain untyped array that 100,000-element pull is cheap and finishes well inside any
reasonable timeout — which is why the unshaped path *looked* lazy already. For a shaped/typed
target it is not: the per-element type check that follows re-walks the whole pulled `Vec`,
re-running the constraint's `where` block (here, `* < 256`) once per element. 100,000 subset-block
evaluations is slow enough to look exactly like a hang, which is what the reported `timeout 5`
repro (and the real-world blocker, `Net::IP::Parse`'s IPv6 constructor,
`my UInt8 @bytes[16]; @bytes[^16] = (loop { 0 });`) hit.

## Fix

`assignment_rhs_values` is now a thin wrapper over a new `assignment_rhs_values_bounded(val,
needed)`, which threads an explicit `needed: usize` through every arm that previously hardcoded
`MAX_LAZY_RANGE_PREFIX` (the four `i64` `Range` variants and the live-coroutine `LazyList` arm).
Every other arm (finite `Array`/`Seq`/`Slip`/`GenericRange`, an already-cached `LazyList`, a
Positional-subclass instance) is untouched and still returns its natural, unbounded length.

The slice-assignment call site now computes `needed` from the LHS key list before pulling the RHS
— `keys.len().max(1)` for the common flat-slice and shaped-1D-multi-index shapes (the `.max(1)`
covers the depth>1 / single-key multidim-cell arm, which type-checks `val`'s own expansion rather
than one value per key), falling back to the original 100,000-element cap only when a key is
itself a nested sublist (`@a[1,(lazy 3,4,5)] = ...`), whose total leaf count this change does not
attempt to compute exactly.

A finite RHS is unaffected either way — every non-bounded arm returns its full content regardless
of `needed`, and a value that violates the element constraint still throws exactly as before
(`Type check failed for an element of @bytes; expected UInt8 but got Int (999)`).

Pinned by `t/collections/subscript/shaped-array-slice-assign-lazy-rhs.t`: a `doesn't-hang` guard
for both the shaped typed and shaped untyped cases, in-process assertions on the stored result, a
finite-RHS constraint-violation check, and a plain-array control.

Closes #8633.
