# A multi's `where` clause now runs exactly once per call

A `where`-constrained `multi` candidate's constraint block used to run
**twice** for a single matching call — once during resolution, and once more
while the winning candidate's own parameter bind re-checked the same
predicate. Rakudo runs it once. A prior fix ([#8720](https://github.com/tokuhirom/mutsu/pull/8720))
had already cut this from four evaluations down to two by removing two
redundant *resolutions* of the same call; this closes the remaining gap
([#8697](https://github.com/tokuhirom/mutsu/issues/8697)).

```raku
my $where-runs = 0;
multi counted(Int $x where { $where-runs++; $x > 0 }) { 'pos' }
multi counted(Int $x) { 'nonpos' }
$where-runs = 0;
say counted(5);      # both: pos
say $where-runs;     # raku: 1     mutsu (before): 2, mutsu (after): 1
```

## Root cause

Resolution of a value-dependent multi (one whose winning candidate carries a
`where` clause) is never cacheable — a winning candidate that itself has a
`where` can never be the cached, type-keyed answer. So whenever such a
winner reaches a call site, resolution just ran that predicate fresh against
these exact arguments a few lines earlier in the same dispatch. The
parameter bind that follows then re-ran the identical predicate against the
identical value while actually binding the parameter — pure duplication of
work already done, and (worse) of any observable side effect the constraint
has.

## Fix

`Interpreter::pending_skip_where_recheck` is a call-scoped one-shot flag,
following the same pattern the interpreter already uses for
`pending_where_exception` and `pending_call_arg_sources`: set right before
invoking a resolved multi winner that carries a `where`, and taken (cleared)
at the top of the parameter binder so it can never leak into a nested call.

Gating the flag on the winner carrying a `where` constraint is a safety
property, not just precision: the positional light-call fast paths refuse
any parameter with a `where_constraint`, so setting the flag structurally
guarantees those paths cannot be the ones to (fail to) consume it — the flag
is always picked up by the general call path, never left dangling for a
later, unrelated call to wrongly trust.

Two independent code paths reach a resolved multi winner and both needed the
fix, found via `rust-gdb` rather than by inspection: `dispatch_func_call_inner`'s
own fast path, and `compile_and_call_function_def` (the interpreter/OTF-compile
fallback, including trivial proto dispatch) — the one a multi whose *both*
candidates carry a `where` reaches. The two are behaviorally identical for a
single `where`-bearing candidate but diverge once a second candidate in the
family also has one, or the winner is `Int:D`-narrowed.

Scoped to positional `where` post-constraints (supplied argument, defaulted
parameter, and omitted-optional binding paths) — the shape of the reported
repro and the dominant case. Named-parameter and hash-slurpy `where` checks
are unchanged.

## Testing

`t/routines/dispatch/multi-candidate-ranking.t` and
`tests/multi_call_resolves_once.rs` are tightened from bounds (`<= 2`, `<= 8`)
to an exact `== 1`, at parity with rakudo. The full `where`/multi-dispatch
test suite (nextsame/callsame redispatch, subset types, sub-signatures,
proto dispatch, closures, omitted-optional binding, defaulted params) stays
green.
