# A promoted element cell reports the bare sigil, not the container's name

ADR-0036 slice 4 made a promoted element container carry its container's
element type constraint, so a wrong-typed write through any alias is now
refused instead of silently landing. The **check** is right everywhere. The
**message** names the container only on the `for`-loop path.

## Repro

```
$ raku  -e 'my Int @a = 1, 2; my $r := @a[0]; $r = "s"'
Type check failed for an element of @a; expected Int but got Str ("s")

$ mutsu -e 'my Int @a = 1, 2; my $r := @a[0]; $r = "s"'
Type check failed for an element of @; expected Int but got Str ("s")
```

Same shape for `%h<a>`, for `(@a[0]:p).value = ...`, and for every other
promotion site. The `for`-loop alias is correct because
`vm_for_loop_alias.rs::name_element_owner` retags the cell with the source name
the loop already resolved:

```
$ mutsu -e 'my Int @a = 1, 2; for @a -> $v is rw { $v = "s" }'
Type check failed for an element of @a; expected Int but got Str ("s")   # matches raku
```

`@` / `%` is not nonsense — it is exactly what raku prints for an anonymous
container (`my $x = (my Int @ = 1, 2); my $r := $x[0]; $r = "s"`) — so the
output is well-formed, just less specific than it should be.

## Why it is like this

`Value::array_slot_ref` / `Value::hash_slot_ref` mint the cell, and they are
`Value` methods: they see the `ArrayData`/`HashData` (hence `value_type`) but
have no idea which variable, if any, the container is reachable through. They
seed `CellConstraint::element_of` with the bare sigil, and a promotion site that
*does* know the name calls `crate::value::retag_element_owner`.

There are ~42 call sites of the two primitives. Only a handful know a name, and
the ones that matter here (`Index`, the `:p`/`:kv` subscript adverbs, the
multi-dim bind descent) sit under opcodes that receive the container as a value
on the stack, with the name already discarded by `GetArrayVar`.

## Two candidate fixes

1. **Carry the name on the container.** Add an owner field next to
   `ArrayData::value_type` / `HashData::value_type`, set where the typed
   declaration is compiled. This is what rakudo does (one `$!descriptor` holds
   `name` and `of` together) and it would fix every site at once, including
   raku's subtle rule that the name is the *declaring* variable, not the alias
   the write came through:

   ```
   $ raku -e 'sub f() { my Int @z = 1, 2; @z }; my @b := f(); my $r := @b[0]; $r = "s"'
   Type check failed for an element of @z; ...      # @z, not @b
   ```

   The cost is that `value_type` has ~21 write sites in `src/`, most of them
   *propagation* (hyper ops, coercions, `methods_mut_dispatch`), and each would
   have to decide whether the owner travels with it.

2. **Retag at the naming opcodes.** Extend the `retag_element_owner` call the
   `for` loop already makes to the `Index` / subscript-adverb / bind-descent
   opcodes, passing the name down from the opcode that resolved the variable.
   Cheaper, but it cannot express the `@z`-not-`@b` rule, and it has to be
   repeated for each new promotion site.

Option 1 is the honest one and probably belongs in ADR-0042's orbit (type
constraints belong to the container, not to a name — this is the *name* half of
the same descriptor). Option 2 is a fine interim if a concrete test starts
depending on the wording.

## Not blocking anything measured

No roast test asserts this message text (checked: `grep -rn 'element of' roast/`
finds only test *descriptions*), and `t/typed-element-alias-constraint.t` pins
the `for` path's exact wording plus the dies/does-not-land behaviour of the
others, so a fix here is a strict improvement rather than a repair.
