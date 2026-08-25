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

## Affected files

- `src/vm/vm_var_index_ops.rs` (`exec_index_op_with_positional`, the itemized-subscript
  normalization that this value never reaches)
- the scalar-assignment store path in `src/vm/vm_var_ops.rs`
