# An immutable Map refuses every removal

A `Map` is immutable, so raku refuses every removal from one:

```raku
Map.new("a", 1).DELETE-KEY("a");   # dies: Can not remove values from a Map
my %h is Map = a => 1;
%h<a>:delete;                      # dies the same way
%h<zz>:delete;                     # ...and so does deleting a key it never held
```

mutsu performed all of them. Nothing on any delete path consulted the
container's immutability — not the `:delete` opcode, not the name-keyed
`DELETE-KEY` method arm, not the value-level one — even though the sibling
*assign* path already refused correctly and the marker was right there:
`HashData::declared_type` is `Some("Map")` however the Map was built.

The gap was easy to miss when reading the delete code, because the quanthash
arms next to it *do* refuse: `Set`/`Bag`/`Mix` raise `X::Assignment::RO`. A Map
is not that — Rakudo dies with a plain `X::AdHoc` whose payload is
`Can not remove values from a Map` — so it needed its own guard rather than
fitting into the read-only branch already there.

There is now one `refuse_map_removal` predicate, called by every delete route
before it touches the container:

- the `:delete` opcode's named form (`%h<k>:delete`, `%h.DELETE-KEY` on a
  lexical), beside the existing `Set`/`Bag`/`Mix` read-only check;
- the opcode's expression form. That path deliberately does *not* check
  `Set`-vs-`SetHash` immutability, because it has no variable metadata to tell
  them apart — but a Map carries its marker on the container itself, so it can
  and does refuse here;
- the value-level `hash_delete_key_value`, which is what a Map reached through
  anything else (a `Capture`'s `.hash`, a mixin) dispatches through. It returns
  `Result` now instead of a bare `Value`.

`Value::is_immutable_map` sees through a `$`-scalar wrapper, so a Map held in a
`$` refuses the same as one bound to a `%`.

Pinned by `t/map-delete-is-refused.t`, whose 12 assertions also pass unmodified
under rakudo. One shape is deliberately asserted only by its effect: for a slice
delete rakudo answers a `List` of `Failure`s (each throwing when used) where
mutsu throws at the subscript, so the test asserts what both agree on — that
nothing is removed.

A mainline `my $m = Map.new("a", 1); $m<a>:delete` still does not refuse, but not
for want of a guard — the identical subscript inside a block does. `:delete`
through a `$`-held container is broken outright when the variable lives in a
local slot rather than in `env`: it removes nothing and leaves the variable
holding `Any`. That is recorded separately in
[`todo/tickets/delete-adverb-on-a-scalar-held-container-destroys-it.md`](../../todo/tickets/delete-adverb-on-a-scalar-held-container-destroys-it.md).
