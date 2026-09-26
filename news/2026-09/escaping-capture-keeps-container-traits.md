# An escaping closure's captured variable keeps its container traits

A closure literal compiled in an escaping position (every closure passed as a
call argument, or stored) promotes the locals it captures and mutates to shared
`ContainerRef` cells. Several store paths read the variable by name, found the
cell instead of the container inside it, and silently skipped the variable's
container traits (#9488):

- `$a is default(42)`: `{ $a = Nil }` stored a raw `Nil`, because the
  `SetGlobal` store (the one a closure or named sub reaches, with no local
  slot) never consulted `var_default` the way `SetLocal` does.
- `%m is Map`: `{ %m<a> = 666 }` wrote into the Map, because
  `container_type_metadata` did not look inside a cell, so the element store
  never saw `declared_type: Map`.
- `my Y @x` (a subset element type): `{ @x[0]++ }` skipped the element type
  check, which only the uncelled `++` path ran; holes a celled `@a[4]++`
  autovivified came out as `(Any)` instead of `(Int)`.
- `{ %h{'c','d'} = 3, 4 }` replaced the whole hash with a fresh one holding only
  the sliced keys, detaching it from the cell every other closure shared.
- `my @a[3, 3]`: `{ @a[3;1] = 1 }` grew the array instead of dying, because the
  multi-dim store's shape check did not look inside the cell.
- QuantHashes: an immutable `Bag`'s `.values`/`.kv` became writable, `%b is Bag`
  could be reinitialized, a `BagHash` weight written through `.values` was lost,
  and a `SetHash[Str]` accepted an `Int` key.
- `%h<a>.VAR` answered `Int` instead of `Scalar` once `%h` was celled.

Each path now reads the variable through the cell (and writes back into it), so
the captured cell reaches the same container descriptor the variable's own
store path uses.

With that in place, the statement call form compiles its positional closure
arguments exactly like the expression form: a positional closure literal is
escaping in both (`Stmt::Call` and the tail-statement call). It used to stay
non-escaping precisely because those roast files regressed otherwise
(`S02-names/is_default.t`, `S32-hash/map.t`, `S09-multidim/*`,
`S09-typed-arrays/*`, the `S02-types` QuantHash files).

`--dump-bytecode` now also lists every closure body, labelled by its path and
whether it escapes; the escaping flag is what decided each of these bugs.

Pin: `t/routines/closure/closure-capture-keeps-container-traits.t`.
