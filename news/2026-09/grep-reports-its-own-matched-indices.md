# `grep` reports its own matched indices instead of re-deriving them by identity

`.grep` dropped a `Proxy` element from its result even when the block returned
`True` for it, and shifted every `:k`/`:kv`/`:p` key after it:

```raku
my $n = 5;
my $p := Proxy.new(FETCH => -> $ { $n }, STORE => -> $, $v { $n = $v });
my $l = (1, $p, 3);

$l.grep({ True }).elems        # raku: 3          mutsu: 2
$l.grep({ $_ > 2 }).join(",")  # raku: 5,3        mutsu: 3
$l.grep({ True }, :k)          # raku: (0, 1, 2)  mutsu: (0, 2)
```

The block was invoked for the `Proxy` element and did see its FETCHed value —
the loss was entirely in the result collection, as the ticket suspected. The
mechanism was not a stray truthiness test, though.

## Root cause

`.grep` over an `Array`/`List` does more than filter. It promotes each matched
*source slot* to a shared element container and hands the result those same
cells, so a writeback loop (`for @a.grep({ $_ %% 2 }) { $_ *= 10 }`) mutates the
source through them. To do that it needs to know which source positions matched
— and `dispatch_grep` re-derived them *after the fact*, by scanning the source
for a value `===` to each result element:

```rust
for needle in filtered_items.iter() {
    if let Some(rel) = mutated_items[scan_from..].iter().position(|candidate| {
        values_identical(candidate, needle)
    }) { ... }
}
```

A `Proxy` element defeats that scan by construction: it reaches the result as
its **FETCHed value** while the source slot still holds the **`Proxy`**, so no
candidate is `===` to the needle. Measured under `rust-gdb`, the scan returned
`indices = [0, 2]` for a three-element match.

A miss there is not merely a lost alias. The result is *rebuilt* from the
located slots — `*data.items_mut() = shared_cells` — so a 3-element result was
overwritten with a 2-element cell list and the element vanished. The same short
index list is what `transform_result` keys `:k`/`:kv`/`:p` from.

## The fix

The grep loop already knows the index it is on; the scan existed only because
`eval_grep_over_items_with_mutated` did not return it. It now does, as
`Option<Vec<usize>>` — `None` for a chunked grep (`grep -> $a, $b { ... }`),
which genuinely has no one-to-one element/slot mapping and where callers keep
their previous behaviour. Every return path reports its own indices: the
full-binding path, the call-carrier path, the compiled fast path (both its
normal and `succeed` exits), and the smart-match path.

The three consumers — the `Array` arm of `dispatch_grep`,
`eval_grep_with_adverb`, and `builtin_grep`'s adverb branch — take the loop's
indices and fall back to the old scan only for the chunked case. A
`debug_assert_eq!` now pins the invariant the old code silently violated: the
promoted cells must cover every matched element, or the result would be
truncated.

Verified against real `raku` across 17 shapes: the headline repro, all four
adverbs, an `Array` holding a `:=`-bound `Proxy` element, writeback aliasing,
duplicate values (which must not collapse the index mapping), smart-match grep,
chunked grep, `Range`/`Seq` sources, and `next`/`last` inside the block.

`first`, `sort` and `unique` were checked at the same time, as the ticket
suggested: they already agreed with rakudo on this input and do not share the
scan.

Pinned by `t/grep-proxy-element.t` (20 rows), which passes identically under
`raku` and mutsu.
