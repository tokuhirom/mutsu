# An element type-check failure names its container again

`#7190` made a promoted element container carry its container's element type
constraint, which is what finally stopped a typed array from silently accepting
a wrong-typed element through an alias. The check was right. The report was not:

```
$ raku  -e 'my Int @a = 1, 2; for @a -> $v is rw { $v = "s" }'
Type check failed for an element of @a; expected Int but got Str ("s")

$ mutsu -e 'my Int @a = 1, 2; for @a -> $v is rw { $v = "s" }'
Type check failed in assignment; expected Int but got Str ("s")
```

Not just a different phrasing — mutsu's form has no symbol at all, so a failure
in a loop over one of several typed containers said nothing about which one. And
`$!.expected` came back as the `Str` `"Int"` rather than the `Int` type object,
so the natural `$!.expected === Int` recovery test failed.

Both are fixed. Every promoted-element path now reports raku's wording, and the
`for`-loop, `:=`-bind, `:p`-pair and `.values`/`.reverse` paths name the
container.

## Why the cell had to learn where it came from

`ContainerCell` had one constraint slot holding a type name. That is enough to
decide whether an assignment is legal, but not enough to word the failure,
because raku words the two origins differently: a typed *scalar* reports
`Type check failed in assignment to $x`, while an *element* reports
`Type check failed for an element of @a` — and names the **container**, not the
alias the write arrived through.

So the slot became `Option<Box<CellConstraint>>` over `{ ty, element_of }` —
rakudo's single `$!descriptor` split into the half that decides legality and the
half that decides blame. Boxing the pair makes the field 8 bytes where the bare
`Option<String>` was 24, so the cell got *smaller*, which matters because
ADR-0045 slice 4 promotes elements eagerly across whole containers.

`array_slot_ref` / `hash_slot_ref` / `hash_autovivify_cell` mint the cells and
are `Value` methods: they can read the container's `value_type` but have no idea
which variable, if any, it is reachable through. They seed `element_of` with the
bare sigil — which is exactly what raku prints for an anonymous container, as
`my $x = (my Int @ = 1, 2); my $r := $x[0]; $r = "s"` shows. Two places then
retag the cell with the real name, both of them places that had already resolved
it for their own routing:

- the `for`-loop element alias, which owns the source name in its plan; and
- the loop's producer-carried-cell arm, so `for @a.values` / `for @a.reverse`
  blame `@a` even though `vm_element_producers.rs` only ever saw a receiver
  value.

`for @a.sort` still reports `@`: it has no source tag at all (ADR-0045 §8
records why routing the producer made one unnecessary), as do the `:=` bind and
the subscript adverbs, whose opcodes receive the container on the stack with the
name already discarded by `GetArrayVar`. Carrying the name properly means
carrying it *on the container*, the way rakudo's descriptor does — filed as
`todo/tickets/promoted-element-cell-does-not-know-its-container-name.md`, which
also records the subtle rule that raku blames the *declaring* variable
(`my Int @z` inside a sub) rather than the alias a caller binds it to.

`hash_autovivify_cell` also seeds the constraint now. No repro is known that
reaches it with a typed container — a typed hash's element cannot be the
intermediate Hash that path promotes past — so this is consistency with its two
siblings rather than a measured fix.

Because it changes what is inside every promoted container, this went through a
full local `make roast` as well as `make test` and the bundled-battery gate.
