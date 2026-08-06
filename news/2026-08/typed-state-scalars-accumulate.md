# Type-constrained state scalars accumulate across calls

```raku
sub f() { state Int $n = 0; $n = $n + 1; $n }
say f(); say f();   # raku: 1 2 — mutsu printed 1 1
```

The #5959 shared-cell storage for `state` scalars carved out
type-constrained ones: they kept the plain store on the theory that every
mutation must flow through the assignment chokepoint for the constraint
re-check. But the plain store is exactly the shape that loses plain-`=`
writes (the assignment writes the local slot while the exit persist reads
the stale env copy), so `state Int $n` never accumulated.

Typed state scalars now live in a `ContainerRef` cell like untyped ones,
with the constraint registered ON the cell
(`register_container_constraint` — the same side table `my T $` anonymous
typed scalars use), so the ContainerRef write chokepoint re-checks it:
`$n = "x"` still dies with `X::TypeCheck::Assignment`, verified for plain
types and `subset` types. One carve-out remains, narrowed from "any
constraint" to native array types (`buf*`/`blob*`/`array[...]`): a
`state buf32 $w` holds a Buf whose element-assignment path (`$w[$j] = ...`
— Digest's SHA2) must see the Buf, not a cell (pinned by
t/digest-battery.t).

This resolves the "type-constrained state scalars" residual of the state
ticket; the ticket now carries only the non-`for` nested-block corner.

Pinned by `t/state-typed-scalar.t` (6 cases, verified against raku).
