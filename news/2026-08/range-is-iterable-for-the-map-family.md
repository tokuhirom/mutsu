# A `Range` is iterable for the `deepmap` family, not a leaf

```raku
say (1..4).deepmap({ $_ * 2 });
```

```
raku : (2 4 6 8)
mutsu: 2..8
```

The block was never called per element. `deepmap_iterate_inner`,
`duckmap_iterate` and `nodemap_iterate` all decide what to descend into by
matching `ValueView`, and mutsu keeps ranges out of the `Array` variant — so a
`Range` fell through to the leaf arm and the *whole* Range was handed to the
block once. `(1..4) * 2` is `2..8`, which is why the wrong answer looked so
plausible.

The consequence went past the value. `deepmap` is a loop construct: it raises
the dynamic loop-handler depth (`runtime/loop_handler_depth.rs`) so a `next` in
the block has somewhere to go. With the Range treated as a leaf the block was
called from *outside* any iteration, so

```raku
say (1..4).deepmap({ next if $_ %% 2; $_ });
```

died with `X::ControlFlow` instead of answering `(1 3)`.

A `Range` now converts to a `List` at the top of each of the three walks, and
the two `is_leaf` predicates (the Array arm's and the Hash arm's) no longer
count one as a leaf, so a nested Range descends and itemizes exactly as a
nested List does — `(1, (2..3)).deepmap(*+1)` is `(2, $(3, 4))` and
`%(a => 1, b => (2..3)).deepmap(*+1)` is `{:a(2), :b($(3, 4))}`. The
conversion goes through `value_to_list`, which caps at `MAX_RANGE_EXPAND`, so an
unbounded range cannot hang the walk. Hypers are built on `deepmap`, and
`(1..4)>>.succ` was already correct — it reaches the walk with the range
already expanded — and stays so.

Pin: `t/deepmap-on-a-range.t`. Found while writing
`t/loop-control-without-loop.t` for
`news/2026-08/loop-control-without-a-loop.md`, which is what made the
`X::ControlFlow` half visible.

One divergence the pin deliberately does not assert against a raku literal:
`duckmap` does not itemize *any* descend, a Range's or a List's
(`todo/tickets/duckmap-does-not-itemize-a-nested-descend.md`). The Range
assertion is written against the equivalent List instead, which is the
invariant this change establishes.
