# Element-typed containers take the capture cell

```raku
my Int @a = 1, 2;
@a.push(3);
my $f = -> { @a.elems };
sub collide() { my Int @a = 9; $f.() }
say collide();     # raku: 3    mutsu: 1
```

Drop the `Int` and mutsu already answered 3. The same held for `my Str @b` and
`my Str %h`: a container carrying an element type constraint was the one shape
ADR-0055's capture cell dichotomy still left hijackable by a same-named
container in whatever frame happened to be calling the closure.

## Why it was refused

An escaping closure that also *mutates* the `@`/`%` lexical it captures cannot
be vouched, so the binding is distinguished by boxing the declaration into a
shared cell (`box_decl_local_container_cell`). That function refused any name
`var_type_constraint` knew about, on the grounds that "typed containers must
keep flowing through the assignment chokepoint" — and that reasoning is sound,
but for a different family. It was written for the **container** type traits:
`my %h is BagHash = a => 1, b => 0, c => 2` builds a plain `Hash` at the
declaration store and lets `ApplyVarTrait` coerce it to the QuantHash
afterwards, reading the slot back to find the initial values. A `ContainerRef`
in that slot is not the `Hash` it looks for, so celling one dropped the
initialiser and `%h` came out with a single key.

Except that those traits never reached this refusal at all. `is BagHash` is
invisible to `var_type_constraint` — which is precisely why
`CompiledCode::compute_free_vars` carries a *separate* `ApplyVarTrait` name
scan that subtracts those names from `needs_cell_unvouched_containers` before
`box_decl_local_container_cell` is ever called. The refusal therefore only ever
caught the **element**-constraint case, and for an ordinary object element type
that constraint is a property of the container (ADR-0042): a write reaching the
array through its cell still re-checks it, so it survives the cell intact.

## The change: narrow the refusal to the NATIVE element types

The refusal is not deleted, because one family really does need it — just not
the one it was written for. A `my atomicint @values` element is a raw machine
slot rather than a `Value`, so a `ContainerRef` in front of the container
breaks native and atomic element access: celling one made
`cas(@values[0], $orig, $orig + $i)` fail with "Cannot convert value to native
integer type 'int'" and abort `roast/S17-lowlevel/cas-int.t` half way through.
That is the same lane `box_decl_local_cell` already declines for scalars via
`legacy_atomic_lane_owns`, so the check now tests for exactly it: the declared
element type is looked up under both the sigilled and the bare spelling, and
the container is refused the cell only when that names a native representation
(`atomicint`, the `int*`/`uint*` widths, `num*`, `str`).

Re-measured before and after: all eight shapes in the family (`my Int @a`,
`my Str @b`, `my Str %h`, `my %h is BagHash`, `my %h is SetHash`,
`my @a is Array[Int]`, the untyped baseline, and a different-sigil trait
sibling) now agree with rakudo v2026.07, where the two element-typed array
shapes previously did not. The QuantHash roast files that motivated the
refusal — `roast/S02-types/{baghash,mixhash,bag,set,mix,sethash}.t` — are all
green, as are all 22 `roast/S17-lowlevel/` and `roast/S09-typed-arrays/` files
(5061 assertions). Removing the `ApplyVarTrait` scan instead was measured to
fail 39/344 subtests in `baghash.t` and 40/295 in `mixhash.t`, confirming which
of the two mechanisms is the load-bearing one for the traits.

## Pins

`t/typed-container-capture-cell.t` (new, 14 assertions, each also passing under
rakudo v2026.07) covers the three element-typed shapes, that the cell does not
cost the element check (the celled array still reports `Array[Int]`, still
rejects a bad `push`, and rejects one made *through* the closure), that the
container traits still coerce, that `cas` on an `atomicint` array element still
works and a native-typed array is still refused the cell, and both the untyped
and the vouched-read-only baselines. Test 13 of
`t/container-capture-cell-dichotomy.t` was rewritten from
"a typed container is left unboxed" — which had no colliding caller and pinned
only the type behaviour — to assert the binding as well.

## Residuals

A native-typed container (`my atomicint @a`, `my int @a`) is still hijackable
by a same-named caller array, since it is the one shape that keeps the refusal.
`t/typed-container-capture-cell.t` pins that it is refused the cell, not that
the hijack is correct.

The `ApplyVarTrait` scan that stays is by NAME across the whole frame, so one
`my %h is BagHash` in any block opts every other `%h` in that frame out of the
cell too. Filed as
`todo/tickets/is-type-capture-cell-exclusion-is-by-name-across-the-frame.md`
with a repro; the real fix there is to make the trait-application path deref the
cell, after which the scan can go.
