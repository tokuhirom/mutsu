# `.map(-> $x is rw { ... })` still misbehaves once the VM-native fast path defers

`src/vm/vm_native_map.rs`'s `try_native_array_map` now rw-aliases an explicit
`is rw`/`is raw` scalar block param to each source element (a transient
`ContainerRef` cell, same pattern as `deepmap_leaf_call`), fixing the common case:

```raku
my @a = 1, 2, 3;
@a.map(-> $x is rw { $x++ });
say @a;   # [2 3 4] now (was: X::Parameter::RW)
```

But `try_native_array_map`'s `classify_body` scanner bails out (`None`, defer to
the interpreter) for any block body containing a loop-control construct
(`next`/`last`/`redo`), `return`, `take`/`emit`, or a phaser. Once deferred, the
call falls through to the interpreter's own map orchestration
(`eval_map_over_items_rw` / `eval_map_over_items`, `src/runtime/resolution_map_grep*.rs`),
which was never taught the same rw-param promotion — it only special-cases `$_`
topic mutation. So a body containing `next` silently drops the writeback instead
of raising (worse than the pre-fix error: it now runs to completion with a wrong
answer):

```raku
my @a = 1, 2, 3, 4, 5;
my @r = @a.map(-> $x is rw { next if $x %% 2; $x++; $x });
say @a;   # raku: [2 2 4 4 6]  mutsu: [1 2 3 4 5] (unchanged)
say @r;   # [2 4 6] both (coincidentally right)
```

## Fix direction

Same ContainerRef-cell promotion, applied to the interpreter's map-rw path
(`eval_map_over_items_rw` in `src/runtime/resolution_map_grep_rw.rs`): when the
block's single positional param carries `is rw`/`is raw`, wrap each element in a
cell before calling (whichever of the two internal sub-paths — the env-insert
fast loop or the `call_sub_value` full-binding loop — actually gets taken), and
write the cell's post-call value back into `list_items[i]`, mirroring the
existing `topic_key` capture/writeback already there for `$_`.

## Affected files

- `src/runtime/resolution_map_grep_rw.rs` (`eval_map_over_items_rw`)
- Also check `src/runtime/resolution_map_grep.rs` (`eval_map_over_items`, the
  non-rw variant) in case a non-`@`-array-backed target (e.g. a `Seq`, `.grep`
  result, or non-lexical array expression) reaches the same rw-param gap through
  a different call site.

Pinned test to extend once fixed: `t/map-native-rw-param.t` (add the
`next`/`last`/`return` cases back once the writeback is correct there too).
