# A promoted element cell takes its owner name from the container descriptor

```
raku  -e 'my Int @a = 1, 2; my $r := @a[0]; $r = "s"'
Type check failed for an element of @a; expected Int but got Str ("s")
mutsu -e '...same...'   # before
Type check failed for an element of @;  expected Int but got Str ("s")
```

ADR-0036 slice 4 made a promoted element container carry its container's element
type constraint, so a wrong-typed write through any alias is refused. The CHECK
was right everywhere; the MESSAGE named the container only on the `for`-loop
path, which retags the cell with the name the loop had already resolved.

## Why it was the bare sigil, and why option 1 turned out to be cheap

`Value::array_slot_ref` / `Value::hash_slot_ref` mint the cell. They are `Value`
methods: they see the `ArrayData`/`HashData` (hence `value_type`) but were told
nothing about which variable, if any, the container is reachable through — so
they seeded `CellConstraint::element_of` with the bare sigil, and only a
promotion site that knew a name could retag it. The ticket weighed two fixes and
called option 1 (carry the name on the container) "the honest one", at the cost
of adding an owner field beside `value_type` and auditing its ~21 propagation
sites.

That field already exists. ADR-0064's container descriptor
(`ArrayData::descriptor_name` / `HashData::descriptor_name`, stamped at the
declaration and reported by `.VAR.name`) is exactly it, and it is right there in
the same `data` the constraint is read from. So option 1 is three lines per
primitive, with no new state and no propagation audit.

It also gets rakudo's subtle rule for free, because the descriptor travels with
the container rather than with the binding: the name is the DECLARING variable,
not the alias the write came through.

```
raku  -e 'sub f() { my Int @z = 1, 2; @z }; my @b := f(); my $r := @b[0]; $r = "s"'
Type check failed for an element of @z; ...      # @z, not @b
```

An anonymous container keeps the bare sigil, which is what rakudo prints for it;
so does the `"element"` sentinel an unsupplied `@`/`%` parameter binds, since
that is a descriptor name but not a variable's.

## Coverage

`t/element-owner-name.t` — 12 assertions, all dual-oracled against rakudo: the
`:=` alias of an array and a hash element, the `:p` adverb's value, the
`for`-loop control, the declaring-name rule through both a returned array and a
whole-array `:=`, the two anonymous-container rows, and four assertions that the
check itself (dies / does not land / a well-typed write still goes through /
an untyped array takes anything) is unchanged. `make test` (3650 files) and a
full local `make roast` (1436 files, 218962 tests) are green.
