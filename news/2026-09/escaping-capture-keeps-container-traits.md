# An escaping closure's captured variable keeps its container traits

A closure literal compiled in an escaping position (every closure passed as a
call argument in expression position, or stored) promotes the locals it
captures and mutates to shared `ContainerRef` cells. Several store paths read
the variable by name, found the cell instead of the container inside it, and
silently skipped the variable's container traits (#9488):

- `$a is default(42)`: `{ $a = Nil }` stored a raw `Nil`, because the
  `SetGlobal` store (the one a closure or named sub reaches, with no local
  slot) never consulted `var_default` the way `SetLocal` does.
- `%m is Map`: `{ %m<a> = 666 }` wrote into the Map, because
  `container_type_metadata` did not look inside a cell.
- `my Y @x` (a subset element type): `{ @x[0]++ }` skipped the element type
  check, which only the uncelled `++` path ran; holes a celled `@a[4]++`
  autovivified came out as `(Any)` instead of `(Int)`.
- `{ %h{'c','d'} = 3, 4 }` replaced the whole hash with a fresh one holding only
  the sliced keys, detaching it from the cell every other closure shared.
- `my @a[3, 3]`: `{ @a[3;1] = 1 }` grew the array instead of dying, and
  `{ @a[2;0]:delete }` did not die either.
- QuantHashes: an immutable `Bag`'s `.values`/`.kv` became writable, `%b is Bag`
  could be reinitialized, a `BagHash` weight written through `.values` was lost,
  and a `SetHash[Str]` accepted an `Int` key.
- A bound List (`my @l := (1, 2, 3)`), a Pair and an immutable Set all accepted
  element stores.
- `my $r := @a[2]` (a deferred element bind): `{ $r = 42 }` skipped the
  element type check.
- `%h<a>.VAR` answered `Int` instead of `Scalar` once `%h` was celled.

The element-assignment op now seeds a free variable's cell contents into the
env for the duration of the store and writes the result back into the cell --
the same seed/restore it already did for a compunit's file-scope lexicals -- so
its many name-keyed checks all see the real container. The other paths read
through the cell individually.

A second, separate bug made the escape analysis promote the wrong variable: a
free variable whose name matched one of the creating frame's locals was treated
as that local even when the local was only declared later, in a sibling block.
`throws-like { Int = 5 }, ...` followed by `{ constant Int = 5 }` then gave the
closure a cell for `Int`, and the assignment went into it instead of dying. The
analysis now honours the slot the closure's creation point actually saw
(`free_var_parent_slots`).

With these fixes the statement call form (`dies-ok { ... }, 'x';`) compiles its
positional closure arguments exactly like the expression form: the one-shot
`stmt_call_positional_closures_nonescaping` compiler flag, which kept them
non-escaping after `ExecCallPairs` was retired (#9462), is gone. The roast files
that regressed without it (`S02-names/is_default.t`, `S32-hash/map.t`,
`S09-multidim/*`, `S09-typed-arrays/*`, the `S02-types` List and QuantHash
files, `S05-substitution/subst.t`) all pass with the arguments escaping.

Running the whole suite that way surfaced one more leak: a fresh `my $a` in an
inner block adopted the outer `$a`'s capture cell through the declaration's
lazy env sync, so `my Int $a; dies-ok { $a = "x" }; { my $a = "s" }` stored the
inner initializer into -- and type-checked it against -- the outer variable. A
declaration no longer adopts an env cell.

`--dump-bytecode` now also lists every closure body, labelled by its path and
whether it escapes.

Pin: `t/routines/closure/closure-capture-keeps-container-traits.t`.
