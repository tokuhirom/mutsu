# Calling `.^name` on a `MAIN`-bound allomorph argument corrupts a later `IntStr.new(...).^name`

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Language/numerics.rakudoc:353`).

## Root cause (unconfirmed — narrowed but not root-caused)

Bisected from the doc's full example down to a minimal 3-line repro. This looks like
order-dependent global/shared-state corruption triggered specifically by invoking
`.^name` inside `sub MAIN($x)` on an argument auto-coerced from `@*ARGS` to `IntStr`.

```raku
@*ARGS = "42";
sub MAIN($x) { say $x.^name; }
say IntStr.new(42, "42").^name;
```

- `raku`: `IntStr` then `IntStr`.
- `mutsu` (`target/debug/mutsu`): `IntStr` then `Str` — the *second* `IntStr.new(...)`
  loses its allomorph tag after `MAIN` ran `say $x.^name` on its own `IntStr` argument.

Bisection notes:

- `IntStr.new(42, "42").^name` alone (no `MAIN` at all) correctly prints `IntStr` — the
  constructor itself is fine in isolation.
- `sub MAIN($x) { $x.^name }` (no `say`, i.e. computing but not printing/saying the name)
  does **not** trigger the corruption — the next `IntStr.new(...).^name` still prints
  `IntStr` correctly.
- `sub MAIN($x) { say $x.^name; }` (with the explicit `say`) DOES trigger it.
- A non-`MAIN` `say $x.^name` on a directly-constructed `IntStr` value (`my $x =
  IntStr.new(...); say $x.^name; say IntStr.new(...).^name;`) does NOT reproduce it —
  `MAIN`'s specific argument-binding/auto-coercion path appears necessary, not just
  "any `say $foo.^name` on an IntStr value".

So the trigger is specifically: `MAIN` auto-coerces an `@*ARGS` string into `IntStr` for
its parameter, and `say`ing that parameter's `.^name` leaves some piece of shared/global
state (perhaps a memoized HOW/mixin-overrides object keyed by type name, given `IntStr` is
implemented as a `Mixin` allomorph) corrupted such that the *next*, unrelated
`IntStr.new(...)` no longer tags its result as the `IntStr` allomorph — it degrades to a
plain `Str`.

## Minimal repro

```raku
@*ARGS = "42";
sub MAIN($x) { say $x.^name; }
say IntStr.new(42, "42").^name;
```

Run as a script (not `-e`, so `MAIN` auto-invokes): prints `IntStr` then `Str` in mutsu,
`IntStr` then `IntStr` in raku.

## Affected files (starting point)

Likely somewhere in the allomorph/`Mixin` construction or `.^name` dispatch path for
`IntStr`/`NumStr`/`RatStr`/`ComplexStr` — search for how `MAIN`'s argument auto-coercion
constructs an allomorph value (probably shares code with `IntStr.new`) and whether any
step mutates a shared/cached structure by reference rather than cloning. A `rust-gdb`
watchpoint on the `IntStr` mixin-overrides map (per the repo's debugging guidelines) once
the corruption window is narrowed further would likely find this quickly — this ticket is
triage-only and does not pin down the exact write site.

## Secondary, distinct finding from the same doc example (not investigated further)

The same finding also showed `< 1/2>.^name` (a space-padded angle-bracket-quoted `Rat`
word) reporting plain `Rat` in mutsu vs. `RatStr` in real `raku` — while the *unpadded*
`<1/2>.^name` is `Rat` in BOTH raku and mutsu (verified directly). So leading whitespace
inside `< ... >` changes whether raku classifies the literal as the `RatStr` allomorph.
This is the same shape as the already-ticketed
[`angle-bracket-quoted-word-space-padded-loses-allomorph.md`](angle-bracket-quoted-word-space-padded-loses-allomorph.md)
(filed for the analogous `< 42/10 >` Complex-adjacent case in `quoting.rakudoc`) — not
re-filed here, just noted as another instance of that same root cause.
