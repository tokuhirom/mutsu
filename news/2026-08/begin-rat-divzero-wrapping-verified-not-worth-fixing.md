# BEGIN-phaser exception wrapping: verified `raku`'s Rat-divide-by-zero gap is a Rakudo quirk, not worth matching

While triaging `t/begin-phaser-begintime.t`, an investigation found that `raku`'s `BEGIN`
phaser wraps exceptions raised inside its body in `X::Comp::BeginTime` — except for one
narrow case. A `die`, a nested sub call that dies, and a method-not-found are all wrapped
consistently in real `raku`:

```
$ raku -e 'use Test; plan 1; sub boom() { die "boom" }; throws-like q[BEGIN { boom() }], X::Comp::BeginTime, "x"'
1..1
ok 1 - x
```

But `BEGIN { my $x = 1 / 0; $x.Int }` is not wrapped:

```
$ raku -e 'use Test; plan 1; throws-like q[BEGIN { my $x = 1 / 0; $x.Int }], X::Comp::BeginTime, "x"'
1..1
not ok 1 - x
# Expected: X::Comp::BeginTime
# Got:      X::Numeric::DivideByZero
```

mutsu wraps all three cases in `X::Comp::BeginTime`, following its general rule that
anything escaping a `CHECK`/`BEGIN` body while `check_phaser_depth > 0` gets wrapped. `1 / 0`
builds a `Rat` lazily in both implementations (`say (1/0).WHAT` prints `(Rat)` with no throw
in either), so the throw site is textually inside the `BEGIN` body in both — yet `raku`'s
own `X::Comp::BeginTime` wrapper, installed around the BEGIN-time `eval` (most likely at the
NQP level), doesn't catch it for this one case. This reads as a Rakudo implementation quirk
in the interaction between lazy `Rat` coercion and whatever depth/scope the wrapper is
applied at, not a documented Raku semantic — every other exception shape stays consistently
wrapped in both implementations.

## Decision: leave mutsu's behavior as-is

Matching `raku` here would mean tagging `RuntimeError`s coming out of `Rat`'s
divide-by-zero check (or more generally out of lazy numeric coercion) so the BEGIN-wrap
logic can special-case skip them — coupling the phaser-wrapping mechanism to `Rat`'s
internal representation for the sake of replicating a single inconsistency in upstream
Rakudo. mutsu's "wrap everything that escapes a phaser body" rule is the more
internally-consistent one, and nothing in roast or common code depends on the narrower
Rakudo behavior. This was a deliberate decision not to chase it, not an oversight.

## Verification (2026-08-21)

Re-ran the exact repro from the original investigation against both `raku` and a fresh
`target/debug/mutsu` build:

```raku
use Test;
plan 3;
sub boom() { die "boom" }
throws-like 'BEGIN { boom() }', X::Comp::BeginTime, 'wraps';
throws-like 'BEGIN { my $x = 5; $x.foo }', X::Comp::BeginTime, 'wraps';
throws-like 'BEGIN { my $x = 1/0; $x.Int }', X::Comp::BeginTime, 'wraps';
```

`raku` still fails only the third assertion (`X::Numeric::DivideByZero` instead of
`X::Comp::BeginTime`); mutsu still passes all three. The divergence and the reasoning for
not chasing it both still hold, so this finding is closed as a verified, deliberate
non-fix rather than an open bug.
