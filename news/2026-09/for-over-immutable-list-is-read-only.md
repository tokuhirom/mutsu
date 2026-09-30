# A `for` loop over an immutable List no longer writes into it

A `List`'s elements are values, not `Scalar` containers, so the alias a `for` loop binds to one of
them is not assignable. mutsu let the write through in two ways: the per-iteration alias promoted
the List's slots to element cells in place, and the per-iteration writeback rebuilt the List from
the loop variable. Both wrote into an immutable List:

```raku
my $l = (1,2);
$_ = 42 for $l.list;                        # raku: X::AdHoc;           mutsu: $l became (42 42)
my @l := (1,2);
for @l.kv -> \k, \v { v = 42 }              # raku: X::Assignment::RO;  mutsu: @l became (42 42)
```

The decision is now made from the source's runtime value, not from how the loop was spelled
(`for_source_is_value_sequence`, which #10261 introduced for `is repr('VMArray')` objects and which
now also answers for any `List`/`ItemList`): `for @l`, `@l.list`, `$l.list`, `@$l`, `@l.kv`,
`@l.values`, `@l.reverse` and a `@`-parameter bound to a List all reach it. The rule is per item, so
a List built from variables keeps its writable items (`my @l := ($a, $b); $_ = 9 for @l` still sets
both).

- **Promotion refuses a List.** `array_is_aliasable` and `aliasable_source_array` now decline
  `List`/`ItemList`, the same way `promotable_array_len` already did for the producers.
- **The topic and a sigilless parameter of a bare item are read-only**: `$_ = ...` is `X::AdHoc`,
  `-> \x { x = ... }` is `X::Assignment::RO` (`ReadonlyKind::ImmutableValue`, the value-level
  error, not a readonly-variable one). An `is copy` parameter owns its own container and is left
  writable.
- **`is rw` / `<->` parameters fail the bind** with `X::Parameter::RW`, whether or not the body
  assigns, for a single parameter and, through the new `ForLoopSpec::multi_param_declared_rw`, per
  chunk slot for a multi-parameter loop (`for @l.kv -> $k, $v is rw`, `for @l <-> $x, $y`). A slot
  that holds a container binds normally. The parser folds "some parameter says `is rw`" into
  `rw_block`, so a `<->` block is recognised as an rw block with no per-parameter `rw` trait.

Pinned by `t/collections/for-immutable-list-source-readonly.t`, every expectation measured against
rakudo. ADR-0045 §8 records this as slice 7.

Left for their own issues: a list-valued *expression* as the source (`(1,2).values`, `.sort`,
sigilless `\x` over a literal) still depends on the compile-time oracle
([#10397](https://github.com/tokuhirom/mutsu/issues/10397)); `List.values` decontainerizes a List
built from variables, so `$_ = 9 for @l.values` over `($a, $b)` now dies where raku aliases the
variables ([#10396](https://github.com/tokuhirom/mutsu/issues/10396)); an immutable `Map`'s
`.kv` with an `is rw` parameter is not rejected
([#10398](https://github.com/tokuhirom/mutsu/issues/10398)).
