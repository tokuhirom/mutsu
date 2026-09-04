# A `for` loop over a scalar binds the variable's container

```
subset SmallInt of Int where -128 <= $_ <= 127;
my SmallInt $a = 1;
for $a -> \x { x = 1000 }
```

- rakudo: `Type check failed in assignment to $a; expected SmallInt but got Int (1000)`
- mutsu: silently set `$a` to `1000`

The write-through itself was already right; only the check was missing, and the
`:=` spelling of the same thing (`my \x := $a; x = 1000`) already threw. So did
the array-source loop (`for @a -> $v is rw`), because an element cell carries
its container's constraint.

## Why the scalar source was the odd one out

A `for` loop over an ARRAY binds the element's container (ADR-0045), so a write
through the parameter goes through the ordinary container chokepoint and is
checked there. A loop over a SCALAR bound the item VALUE and relied on a
source-variable writeback, `store_loop_source_var`, which writes straight into
the local slot and `env`:

```rust
self.env_mut().insert(target.to_string(), value.clone());
match source_var_locals.get(idx).copied().flatten() { Some(slot) => … }
```

No constraint lookup, no chokepoint — so nothing could check it.

The ticket set out two fixes and asked for the second: type-checking inside the
writeback needs `Result` threaded through six call sites reached from inside the
loop's `next`/`last`/`redo` control flow, while making the parameter a real
alias removes the writeback altogether and lets the existing chokepoint do the
work. That is what this does — one mechanism replacing two, which is the shape
ADR-0045 points at.

## The slice

A new `ForElementAlias::ScalarVar` promotes the source variable to a shared
`ContainerRef` cell (inheriting its declared `of`-type, exactly as the `:=`
bind promotion does) and binds the parameter to it. `binding_carries_element_cell`
then retires the writeback for that iteration, as it already does for every
promoted element.

Two discriminators earn their place, both found by measurement rather than
reasoning:

- **The tag's sigil.** `TagContainerRef` spells a plain scalar source with NO
  sigil (`for $a` tags `"a"`), while the deref'd-container shape `for @$s` tags
  `"$s"` and a direct array tags `"@a"`. A bare name is exactly "a scalar
  variable, which is not a container of elements".
- **The item must be the variable's own value.** `for $pair.value -> $v is rw`
  tags the same bare name and also yields one item — but that item is the pair's
  value, and aliasing the variable there replaced the whole `Pair` with it
  (`roast/S04-blocks-and-statements/pointy-rw.t`). The scalar twin of the
  `items_are_source_elements` guard the array path already applies. A mutable
  QuantHash's `for $b.values` is declined by `values_mode` for the reason
  ADR-0045 §2.4 already records: a weight is not a stored element container.

## Coverage

`t/for-scalar-source-alias.t` — 20 assertions, all dual-oracled against rakudo:
the type check in all four parameter shapes (`\x`, `is rw`, `<->`, and a plain
`Int`), the `:=` control, seven rows of well-typed writes still landing
(including the implicit topic, two writes in one iteration, and an undefined
scalar), the read-only `-> $x` binding, and four rows for the shapes that must
NOT alias the variable (`.value`, a BagHash `.values`, `for @$s`, and a scalar
holding an array). `make test` (3651 files) and a full local `make roast` (1436
files, 218962 tests) are green.

`Native::Overflow`'s `t/01-basic.rakutest`, the named consumer the ticket
recorded, uses the loop spelling and was blocked on exactly this half.
