# An undefined `state Int $x` reads as the `Int` type object

```raku
sub t() { state Int $u; $u.^name }
say t();   # raku: Int   mutsu: Nil
say t();   # raku: Int   mutsu: Nil
```

The `my` spelling was already right (`my Int $z; $z.^name` is `Int` in both),
which is what pinned the cause: `SetVarType` seeds a typed scalar whose value is
`Nil` with the constraint's type object (`typed_scalar_nil_seed_value`), and it
does so for a `state` declaration exactly as for a `my` one — but the `state`
declaration then goes on to emit `StateVarInit`, which installs the state
store's value for the slot **over** that seed. On the first entry that value is
`Nil`, and on every later entry it is the persisted `Nil`, so the seed never
survived a single call.

## The fix

`StateVarInit`'s contract is "install the persisted value for this slot", and
that contract is what makes `state` persistence work, so the seeding moved
*inside* it rather than the op learning to skip anything: on the first
initialization of a **scalar** with a type constraint and a `Nil` initializer,
the same `typed_scalar_nil_seed_value` supplies the stored value. The persisted
value is therefore the type object from the start, which is why the second call
answers `Int` too.

The neighbourhood the ticket listed was measured against `raku` and is unmoved:

- `state Int $x = 0` — the initializer is not `Nil`, so it still wins, and still
  persists as `0`.
- `state $s` untyped — no constraint, so the arm is not taken; still `Any`.
- `state Int @a` / `state Int %h` — containers, which this arm does not reach
  (the existing `ContainerTypeInfo` tagging below handles them); still
  `Array[Int]` / `Hash[Int]`.
- `state Int $c` in a loop body — `Int` on every iteration.
- A typed `state` counter (`$p = ($p // 0) + 1`) — still 1, 2, 3.
- `state buf8 $w` now seeds the same way `my buf8 $w` does, which is the point:
  the two spellings agree.

Pinned by `t/state-typed-scalar-reads-as-type-object.t`, whose 12 assertions
pass unchanged under rakudo. `t/state-and-our-typed-declarations.t` and the
other 12 `t/state-*` files stay green, as do
`roast/S04-declarations/{state,my-6e,our,smiley,multiple}.t` and
`roast/6.c/S04-declarations/my-6c.t`.
