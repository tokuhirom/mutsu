# `raku` does not wrap a Rat div-by-zero inside `BEGIN` as `X::Comp::BeginTime`

Found while triaging `t/begin-phaser-begintime.t`
(`todo/tickets/local-tests-rely-on-a-lenient-native-is.md`).

`raku`'s `BEGIN` wraps an exception raised inside its body in
`X::Comp::BeginTime`, and mutsu matches that for a plain `die`, a nested sub
call that dies, and a method-not-found:

```
$ raku -e 'use Test; plan 1; sub boom() { die "boom" }; throws-like q[BEGIN { boom() }], X::Comp::BeginTime, "x"'
1..1
ok 1 - x
```

But `BEGIN { my $x = 1 / 0; $x.Int }` does not:

```
$ raku -e 'use Test; plan 1; throws-like q[BEGIN { my $x = 1 / 0; $x.Int }], X::Comp::BeginTime, "x"'
1..1
not ok 1 - x
# Expected: X::Comp::BeginTime
# Got:      X::Numeric::DivideByZero
```

mutsu wraps it (`X::Comp::BeginTime`), which is the internally-consistent
"wrap everything" rule but disagrees with real `raku` on this one case.

## Why this is not a spec requirement worth chasing

`1 / 0` builds a `Rat` lazily in both `raku` and mutsu — neither throws at the
division, only at `.Int` (confirmed with `tmp/div0-plain.raku`-style probes:
`say (1/0).WHAT` prints `(Rat)`, no throw, in both). So the throw site is
textually inside the `BEGIN` body in both implementations, yet `raku`'s
`X::Comp::BeginTime` wrapper — installed around the BEGIN-time `eval`, most
likely at the NQP level — doesn't catch it. This reads as a Rakudo
implementation quirk (some interaction between the lazy Rat coercion path and
the depth/scope the wrapping is applied at), not a documented Raku semantic.
Every other kind of exception (`die`, method-not-found, a runtime error from
inside a called sub) *is* consistently wrapped in both implementations, so
mutsu's general rule ("anything that escapes a CHECK/BEGIN body while
`check_phaser_depth > 0` is wrapped") is architecturally the right one — this
is a single narrow exception to it in upstream Rakudo.

## What it would take to match

Distinguish "exception raised directly while executing statements inside the
phaser body" from "exception raised later by a lazily-coerced value that
happens to still be inside the phaser's lexical extent" — i.e. tag
`RuntimeError`s coming out of `Rat`'s div-by-zero check (or more generally,
out of lazy numeric coercion) so the BEGIN-wrap check can skip them. That
couples the phaser-wrapping mechanism to `Rat`'s internal representation for
one exception type, for close to zero real-world benefit (nothing in roast or
common code relies on this). Not worth it unless a roast test surfaces the
same gap with broader impact.

## Repro

`tmp/begin-nested-die2.raku`-style probe (not checked in):

```raku
use Test;
plan 3;
sub boom() { die "boom" }
throws-like 'BEGIN { boom() }', X::Comp::BeginTime, 'wraps';                  # raku: ok
throws-like 'BEGIN { my $x = 5; $x.foo }', X::Comp::BeginTime, 'wraps';       # raku: ok
throws-like 'BEGIN { my $x = 1/0; $x.Int }', X::Comp::BeginTime, 'wraps';     # raku: NOT ok
```
