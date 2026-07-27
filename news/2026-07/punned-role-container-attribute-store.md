# A punned role's `@`/`%` attributes live in the instance cell, and an `is <Role>` container survives element assignment

A bare role instantiated directly (`R.new`, which puns the role to a class) is
represented as a `Mixin` wrapping an instance. Its *scalar* attributes were
seeded into the wrapped instance's shared attribute cell — the store of record
for every `$!x` read and write inside a role method — but its `@`/`%`
attributes were deliberately not: they lived only as `__mutsu_attr__<name>`
entries in the mixin map, with a `TODO` in `dispatch_new` recording the
intention to converge them. So an ordinary container mutation inside a role
method was dropped:

```raku
role R { has %!h; method poke { %!h<k> = 1 }; method peek { %!h.raku } }
my $r = R.new;
$r.poke;
$r.peek;      # raku: {:k(1)}   mutsu (before): {}
```

`%!h<k> = 1` writes through `write_attr_cell_by_key`, which only writes
attributes the cell already holds, and the cell held no `h`. Meanwhile the
`handles` delegation path mutated the marker, rebuilt the whole `Mixin` and
stored it back into the **caller's env variable** — a writeback that reaches an
object held in a plain lexical but not one held in an attribute or a collection
element. Two stores, each load-bearing for a different set of tests.

The cell is now the single store for every sigil. Construction seeds `@`/`%`
attributes into it (an unsupplied one gets an empty container, not `Nil`, and is
tagged with its declared element/key types the way the class path's
`seed_attr_value` does), and the delegated element assignment — extracted into
`assign_role_mixin_element` — refreshes each marker from the cell before
mutating and writes the result back into it. The env writeback survives only for
a plain lexical, which is the one case the cell cannot stand in for.

That let three further gaps in the same feature fall out, all needed by
`DBDish::TypeConverter`:

- **The delegation to apply is chosen by the delegate container, not by the
  index's Rust type.** An object-hash key is a `Package` (`$conv{Str} = …`), and
  testing the index for `Str` sent it to the `ASSIGN-POS` branch and then out of
  the delegation path entirely — silently replacing the role object with a plain
  `Hash`. The delegate hash also owns the keying rule now, so an object-hash
  delegate is `.WHICH`-keyed and records the original key object.
- **`handles <AT-KEY>` alone makes the subscript assignable.** raku's `AT-KEY`
  yields the delegate's own rw container, so `$obj{k} = v` stores through it with
  no `ASSIGN-KEY` declared — which is exactly what `TypeConverter` relies on.
  Reading such a subscript with a non-`Str` key dispatches `AT-KEY` too; only the
  `Str` arm did before, so a type-object key read as `Nil`.
- **Assignment through an accessor reaches it.** `$obj.Converter{Int} = $sub`
  goes through the method-lvalue path, which handled an `Instance` but not a
  `Mixin`; it now shares `assign_role_mixin_element`.

Two related fixes came with it. Invoking a **type object held in a variable**
now coerces — `my $t = Int; $t('123')` is `123`, and `$type($datum)` is how a
coercion table applies its fallback — instead of dying with "No such method
'CALL-ME'"; the bare-name form (`Int("123")`) already did this, and a type object
reached through a variable took a different dispatch. And
`get_attr_type_constraint` falls back to the role registry for a class name that
is still only a role, so a punned role's `has Int %!t{Mu:U}` resolves its key
type; punning eagerly instead would have changed how a later `class C does R`
composes.

Pinned by `t/punned-role-container-attribute.t`, with
`t/positional-role-attr-writeback-coherence.t` unchanged as the coherence pin for
the delegation path.

With this, `DBIish`'s `t/06-types.rakutest` passes 12/12 and the battery reaches
raku parity on 8 of its 9 generic/SQLite files; the last one, `01-basic`, waits
on `BODY_OF` (`todo/deep/nativehelpers-blob-moarvm-guts.md`).
