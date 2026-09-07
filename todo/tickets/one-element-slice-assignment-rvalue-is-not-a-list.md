# A one-element slice assignment's rvalue is the bare element, not a one-element list

Found 2026-09-07 while making a slice assignment's rvalue the list it actually
stored (`news/2026-09/short-rhs-slice-assign-broadcasts-instead-of-padding.md`).
Pre-existing and independent of that change — verified against the same tree with
the change stashed — so it was deliberately left alone there.

## Repro

```
$ ./target/debug/mutsu -e 'my @d; say (@d[0,] = 5).raku; say (@d[0,] = 5).^name'
5
Int

$ raku -e 'my @d; say (@d[0,] = 5).raku; say (@d[0,] = 5).^name'
(5,)
List
```

`.elems` agrees (1 on both), so only the *shape* of the rvalue is wrong: raku
keeps the one-element **list** a slice always produces, mutsu collapses it to the
element.

The controls hold and are what make this narrow: a genuine single-index
assignment yields the bare value on both (`(@d[0] = 5)` is `5`, `(%h<a> = 5)` is
`5`). A one-element *slice* (`@d[0,]`, with the trailing comma) is a different
construct — it names a list of one slot — and that is the only shape that
diverges.

## Where to look

`src/vm/vm_var_assign_index_named.rs`, the `idx_is_single_element` arm of the
`ValueView::Array(keys, kind)` slice branch:

```rust
let result = if let Some(nested) = nested_result {
    nested
} else if idx_is_single_element {
    Self::itemize_value(val)          // <-- collapses the slice to its element
} else if !assigned_values.is_empty() {
    Value::array(assigned_values)
} else {
    val
};
```

`idx_is_single_element` is doing double duty. It exists so an *itemized* hash
index (`my $s = $(1,2); %c{$s}`) is one key rather than a slice — see the long
comment on the `!kind.is_itemized()` guard just above the arm — and that job is
load-bearing. Reusing the same flag to decide the *rvalue shape* is what makes a
one-element positional slice render as a scalar.

The shared tail at the end of the same function has a second copy of the rule
(`idx_is_single_element && !var_name.starts_with('%')`), so both need the same
answer.

## Fix sketch

Separate "this subscript names one key" (the itemized-index question, which
`idx_is_single_element` must keep answering) from "this assignment's rvalue is a
scalar" (true for `@a[0] = …`, false for `@a[0,] = …`). The distinguishing
information — whether the subscript was written as a list — is present at the
opcode's index value, since `@d[0,]` arrives as a one-element non-itemized
`Array` while `@d[0]` arrives as a bare `Int`; the arm just does not consult it.

## Check when fixing

`(@d[0] = 5)` and `(%h<a> = 5)`, which must stay bare; `(@d[0,] = 5)` and
`(%h<a>, = 5)`; the itemized-key control from the guard's own comment
(`my $s = $(1,2); my %c; %c{$s} = "x"; %c.keys`), which must still be one key;
`roast/S09-subscript/slice.t` and `roast/S32-hash/adverbs.t`; and
`t/slice-assign-short-rhs-padding.t`, whose rvalue rows pin the multi-element
side of this arm.
