# A `Range` assigned to a named scalar is not itemized, so it slices instead of indexing

Found while fixing `todo/tickets/array-subscript-range-var-list-context-slip.md`
(runtime slice-vs-element decision for `for @a[$subscript]`).

## Root cause

Raku itemizes the right-hand side of a scalar *assignment*, so a `Range` stored in a
`$` variable is one item, and using it as a subscript is a SINGLE index that numifies
to the range's element count. `mutsu` keeps the bare `Range` (no `ValueView::Scalar`
wrapper) for a *named* scalar, so the subscript is treated as a slice selector.

The itemization machinery itself works — `src/vm/vm_var_index_ops.rs`'s
`exec_index_op_with_positional` already normalizes a `ValueView::Scalar(Range | Array)`
subscript to its element count, and an *anonymous* `my $ = 1..3` hits that path
correctly. Only the named-scalar store loses the `Scalar` wrapper.

## Minimal repro

```raku
my @n = <4 8 15 16 23 42>;
my $assigned = 1..3;
say @n[$assigned].raku;      # raku: IntStr.new(16, "16")   mutsu: (8, 15, 16)
say @n[my $ = 1..3].raku;    # raku AND mutsu: IntStr.new(16, "16")  -- already OK
say @n[$(1,2)].raku;         # raku AND mutsu: IntStr.new(15, "15")  -- already OK
```

A bound range (`my $r := 0..2`) correctly stays a slice selector in both, so the
divergence is specific to `=` assignment into a named `$` variable.

## Why it is not a one-liner

This is the same family as `todo/deep/element-itemization-lost-in-scalar-binding.md`:
mutsu does not consistently model "a scalar container holds one item". Naively wrapping
every scalar-assigned `Range` in `ValueView::Scalar` would touch every consumer that
pattern-matches on `ValueView::Range*` after reading a `$` variable (arithmetic,
`.min`/`.max`, smart-match, `for` iteration of a bound range, ...), so it needs the
container-model work rather than a local patch at the subscript site.

## Re-verified 2026-08-26

Still reproduces exactly as written (`@n[$assigned].raku` yields
`(8, 15, 16)`; the anonymous `my $ = 1..3` and `$(1,2)` forms are still
correct). Confirmed as a member of
`todo/deep/element-itemization-lost-in-scalar-binding.md` — that file now names
this ticket as one of its blocked dependents. Do not patch the subscript site.

## Status after ADR-0040 slice 2 (2026-08-27): still open, and now clearly out of scope

ADR-0040's slices 1-2 put itemization at the `Array`/`Hash` **element** store, which is a
different store from the one this ticket is about. Re-measured on the slice-2 build:

```
my @n = <4 8 15 16 23 42>; my $assigned = 1..3; say @n[$assigned].raku
  raku : IntStr.new(16, "16")
  mutsu: (IntStr.new(8, "8"), IntStr.new(15, "15"), IntStr.new(16, "16"))   # unchanged
```

The value here never enters an `Array`/`Hash` element, so no hook ADR-0040 places can see
it: what is missing is itemization at the **named `$` scalar assignment** store. The
primitive ADR-0040 shipped (`Value::itemize_for_element_store`, which already covers every
`Range` shape) is directly reusable, so the remaining work is the store site plus the
consumer audit the "Why it is not a one-liner" section describes — not new machinery.
Note that ADR-0040's `Range` arm confirms the itemization of a stored `Range` is
observable and correct, which removes the "is this even the right model?" half of the
question.

## Affected files

- `src/vm/vm_var_index_ops.rs` (`exec_index_op_with_positional`, the itemized-subscript
  normalization that this value never reaches)
- the scalar-assignment store path in `src/vm/vm_var_ops.rs`
