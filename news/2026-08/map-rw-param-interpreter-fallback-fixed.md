# `.map(-> $x is rw { ... })` also writes back on the interpreter's fallback path

The VM-native `.map` fast path (`try_native_array_map`,
`src/vm/vm_native_map.rs`) learned to rw-alias an explicit `is rw`/`is raw`
scalar block param to each array element, but it still defers to the
interpreter's own map orchestration (`eval_map_over_items_rw`,
`src/runtime/resolution_map_grep_rw.rs`) for anything its conservative
`classify_body` scanner cannot prove safe -- a typed/constrained param
(`-> Int $x is rw { }`), or a body containing `next`/`last`/`return`/`take`/a
phaser. That interpreter path was never taught the same promotion, so a
deferred call silently dropped the writeback instead of mutating the source:

```raku
my @a = 1, 2, 3, 4, 5;
my @r = @a.map(-> $x is rw { next if $x %% 2; $x++; $x });
say @a;   # raku: [2 2 4 4 6]   mutsu (before): [1 2 3 4 5] (unchanged)
say @r;   # [2 4 6] both (coincidentally right)
```

## Fix

`eval_map_over_items_rw` has two internal loops depending on the block's
signature complexity:

- The `call_sub_value`-based loop (typed/constrained/`where`-clause params,
  routine callbacks, composed callables): each element is now wrapped in a
  transient `ContainerRef` cell (the same `deepmap_leaf_call` pattern used
  for the VM-native path) and passed as the sole argument when the single
  param carries `is rw`/`is raw`; the cell's post-call value is written back.
- The env-insert fast loop (plain untyped params, including bodies with
  `next`/`last`/`redo` that only this loop's control-flow handling supports):
  the rw param's env slot is bound to a fresh `ContainerRef` cell instead of
  a plain value before each `vm.run_reuse` call, and the cell is read back
  after -- both on normal completion and on a `next`/`last` signal, mirroring
  the existing `$_`-mutation (`topic_key`) writeback right next to it.

Extends `t/map-native-rw-param.t` with the typed-param and `next`/`last`
cases (verified against `raku`).
