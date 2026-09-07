# An undefined `state Int $x` reads as `Nil`, not as the `Int` type object

Measured 2026-09-07 while fixing
`todo/tickets/state-and-our-typed-declaration-hoist.md`
(`news/2026-09/state-typed-declarations-hoist-and-our-typed-declarations-are-refused.md`).
Verified to reproduce identically before and after that change, so it is
independent of it.

## Repro

```raku
sub t() { state Int $u; $u.^name }
say t();   # raku: Int   mutsu: Nil
say t();   # raku: Int   mutsu: Nil
```

The `my` spelling is already correct:

```raku
sub m() { my Int $z; $z.^name }
say m();   # both: Int
```

## Root cause

`SetVarType` seeds a typed scalar whose value is `Nil` with the constraint's
type object (`typed_scalar_nil_seed_value`), which is what makes `my Int $z`
read as `Int`. For a `state` declaration the seed is then overwritten: the
declaration goes on to emit `StateVarInit(slot, key)`, which installs the
state store's value for the slot — `Nil` on the first entry, and the persisted
`Nil` on every later one.

So the fix is for the state store's INITIAL value of a typed scalar to be the
type object rather than `Nil` — i.e. `StateVarInit` has to know the
declaration's constraint, or the seeding has to happen after it rather than
before.

## Why it is a ticket

`StateVarInit`'s contract is "install the persisted value for this slot", and
`state` persistence is exactly the thing a careless change here breaks (the
`state` counter/accumulator rows in
`t/state-and-our-typed-declarations.t` are the guard). Threading the
constraint into the op, or reordering the seed against it, wants its own
measurement across the shapes: a typed `state` with and without an
initialiser, `state Int @a` / `%h` (containers, which the seed does not touch),
`state Int $x = 0` (whose initialiser must still win), a `state` in a loop body,
and `state $s` untyped (which must stay `Any`, not become anything else).

## Acceptance

The repro answers `Int` on both calls; `t/state-and-our-typed-declarations.t`
stays green, including every persistence row; and `roast/S04-declarations/*.t`
does not move.
