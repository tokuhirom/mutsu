# A single `Buf`/`Blob` replacement argument to `.splice` does not flatten

Found while fixing `.splice`'s one-arg rule (see
`news/2026-08/splice-replacement-arg-one-arg-rule.md`), by sweeping every
`Positional`-ish single argument against real `raku`. Every kind swept now
agrees except `Buf`.

## Repro

```raku
my @a = 1,2,3;
@a.splice(1,1,Buf.new(1,2));
say @a.raku;
```

- raku: `[1, 1, 2, 3]` (the buf flattens to its elements)
- mutsu: `[1, Buf.new(1,2), 3]` (kept as one element)

## Root cause

`Blob` does `Positional`, so a lone `Buf` argument binds Rakudo's
`(..., @new)` `splice` candidate and contributes its elements, exactly like a
lone `Array`/`List`/`Seq`/`Range` does.

mutsu's shared helper `flatten_splice_replacement_args`
(`src/runtime/mod.rs`) implements the one-arg rule over the `ValueView`
variants that `crate::runtime::utils::value_to_list` can expand, and
`value_to_list` has no `BufStorage` arm — it returns nothing useful for a buf,
so the helper deliberately leaves `Buf` out of its `Positional` set rather
than guess.

## Why it was left out of that fix

Decoding a `BufData` back into element `Value`s is not a one-liner: the node
stores raw `bytes` plus a `width` (1/2/4/8) and an `ElemKind`, and a `Buf` can
also arrive as an `Instance` with attributes (`buf_elems` in
`src/value/value_buf.rs`) rather than as a `ValueView::BufStorage`. Getting
that right is a `value_to_list` change with its own blast radius (every
`Buf`-in-list-context caller), not a `splice` change, and splicing a `Buf`
into an `Array` is an exotic call.

## Suggested fix

Give `crate::runtime::utils::value_to_list` a `BufStorage` arm that decodes
elements at the node's own `width`/`ElemKind` (reusing whatever
`value_buf.rs` already uses for `.list`/`.values` on a buf), then add
`ValueView::BufStorage(_)` to the single-argument `Positional` set in
`flatten_splice_replacement_args`. Pin it with a row in
`t/splice-arg-flatten-rule.t` and check the `Buf`-in-list-context callers of
`value_to_list` for fallout.
