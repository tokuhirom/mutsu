# `.map(-> $x is rw { ... })` over a concrete array now mutates the source

```raku
my @a = 1, 2, 3;
@a.map(-> $x is rw { $x++ });
say @a;   # [2 3 4] now (was: X::Parameter::RW: 'x' expects a writable variable argument)
```

Raku passes each array element's container to the map block, so an `is rw`
(or `is raw`) block parameter mutates the element in place — the same
rw-binding `@a.map({ $_++ })` already got via the implicit topic `$_`. The
VM-native `.map` fast path (`try_native_array_map`,
`src/vm/vm_native_map.rs`) never got that promotion for an *explicit* scalar
param: it passed each element as a plain `Value` with no source name and no
`ContainerRef` cell, so the general binder's rw check
(`X::Parameter::RW` for a non-lvalue argument) rejected the block outright.

## Fix

When the block has exactly one positional param (arity 1) carrying an
`is rw`/`is raw` trait, wrap each source element in a transient
`ContainerRef` cell before calling — the same pattern `deepmap_leaf_call`
already uses for `@a.deepmap(-> $x is rw { $x++ })` — and write the cell's
post-call value back into the source array, mirroring the existing `$_`
topic-mutation writeback right next to it. The general binder already treats
a bare `ContainerRef` argument as a writable lvalue, so no other plumbing was
needed; this only had to happen at the call site.

## Residual gap

`try_native_array_map` still defers to the interpreter's own map
orchestration for any block body containing loop control (`next`/`last`),
`return`, `take`/`emit`, or a phaser (its `classify_body` scanner is
conservative about anything that could escape the loop). That interpreter
path was never taught the same rw-param promotion, so a deferred body with
`next`/`last` still silently drops the writeback instead of mutating the
source. Tracked in
`todo/tickets/map-rw-param-interpreter-fallback-still-silent.md`.

Pinned by `t/map-native-rw-param.t`.
