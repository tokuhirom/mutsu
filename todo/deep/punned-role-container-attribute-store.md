# A punned role keeps its `@`/`%` attributes in mixin markers, not in the instance cell

A bare role instantiated directly (`R.new`, which puns the role to a class) is
represented as a `Mixin` wrapping an instance. Its *scalar* attributes are
seeded into the wrapped instance's shared attribute cell — the store of record
for every `$!x` read and write inside a role method — but its `@`/`%`
attributes are deliberately **not**: they live only as `__mutsu_attr__<name>`
entries in the mixin map. The guard and its reasoning are in
`src/runtime/methods_object_dispatch_new.rs` (the `registry().roles.get(...)`
arm of `dispatch_new`), which already carries a `TODO: compile the container
case onto the cell too`.

The consequence is that an ordinary container-attribute mutation inside a role
method is lost:

```raku
role R { has %!h; method poke { %!h<k> = 1 }; method peek { %!h.raku } }
my $r = R.new;
$r.poke;
$r.peek;      # raku: {:k(1)}   mutsu: {}
```

`%!h<k> = 1` writes through `write_attr_cell_by_key`
(`src/vm/vm_var_assign_computed_attr.rs`), which only writes attributes the cell
already holds — and the cell holds no `h` — so the write is dropped silently.
The same role composed into a class (`class D does R { }`) works, because the
class path seeds every sigil into the cell.

## Why the markers cannot simply be dropped

The `handles` delegation path reads and writes the markers, not the cell:
`delegated_role_attr_key_from_mixins` (`src/runtime/types/roles.rs`) resolves a
delegated `AT-POS`/`ASSIGN-KEY` to a `__mutsu_attr__` key, and the element
assignment in `src/vm/vm_var_assign_index_named.rs` mutates the marker, rebuilds
the whole `Mixin` value, and stores it back into the *caller's env variable*.
That writeback reaches an object held in a plain lexical but not one held in an
attribute or an array element. `t/positional-role-attr-writeback-coherence.t`
pins the current behaviour of exactly this path.

So today there are two stores that disagree, and each is load-bearing for a
different set of tests. Seeding containers into the cell as well (the obvious
one-line change) makes them diverge rather than converge.

## The fix

Make the instance cell the single store for every sigil, and rework the
delegation forwarder to resolve its target attribute through the cell instead of
the mixin map. The `__mutsu_attr__` markers then become construction-time seeds
plus "is this a role mixin" flags, which is what the scalar case already treats
them as. There are ~36 `__mutsu_attr__` sites across 12 files; the load-bearing
ones are `runtime/types/roles.rs`, `vm/vm_var_assign_index_named.rs`,
`runtime/methods_mut_method_lvalue.rs`, `runtime/builtins_multidim_subscript.rs`
and `runtime/methods_call_dispatch.rs`. Removing the env-variable writeback in
favour of a cell write is what makes the object usable from an attribute.

## Why it matters beyond the repro

`DBIish`'s `t/06-types.rakutest` needs it: `role TypeConverter does Associative
{ has Callable %!Conversions{Mu:U} handles <AT-KEY EXISTS-KEY> }` is punned by
`has %.Converter is DBDish::TypeConverter`, so the punned object lives in an
*attribute* and both stores are exercised at once. See
`todo/tickets/dbiish-blockers.md` ⑤. `todo/tickets/punned-role-ignores-user-new.md`
is the same construction path from a different angle and should be fixed with
it.
