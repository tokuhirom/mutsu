# One function call, one multi resolution

`dispatch_func_call_inner` used to run the full `resolve_function_with_types`
candidate walk **three times** for a single `multi` call. For a `multi` whose
candidates are type+arity deterministic the second and third walks were served
from `func_multi_resolve_cache` and merely wasted a few hash lookups. For a
*value-dependent* one — a `where` clause, a subset, a literal, a coercion —
`func_multi_dispatch_type_cacheable` correctly refuses to cache the winner, so
all three walks ran the constraint, i.e. **user code**:

```raku
my $n = 0;
multi sub f(Int:D $x where { $n++; True }) { 'int' }
multi sub f($x where { $n++; True })       { 'any' }
my $r = f(1);
say "$r $n";   # rakudo: "int 1".  mutsu: "int 16".
```

The three sites, all inside one `dispatch_func_call_inner` invocation:

1. `find_compiled_function_memo` → `resolve_function_multi_cached`;
2. the `has_multi_candidates_cached` arm, re-resolving by name;
3. `compile_and_call_function_def` → `push_multi_dispatch_frame`, resolving by
   name a third time to work out which candidate it had just been handed.

## Site 1 already had the machinery; it was withholding it

The `memo` out-parameter added in #7573 exists precisely to hand site 1's answer
to site 2, but it was filled **only when the resolution came from the type-keyed
path**. The reasoning (recorded on the now-folded-away
`resolve_function_multi_cached_keyed` twin) was
that only a type-keyed answer is a pure function of
`(package, name, argument type keys)`, whereas the un-keyed fallback also reads
`pending_call_arg_sources` — an `is rw` parameter accepts only a writable
lvalue — so two resolutions of the same call under different pending sources may
legitimately differ. That is a sound rule for two resolutions in *different*
dispatches; it is vacuous inside one. Both consumers of the memo resolve and
consume within a single dispatch of a single call: in `OpCode::ExecCallPairs`
nothing touches the pending sources in between, and in `dispatch_func_call_inner`
the probe runs with the call's own `arg_sources` installed while the consumer ran
with them already cleared — so the memoised answer is in fact the *more* accurate
of the two. The memo is now filled for every resolution.

Its gate had the exact inversion that makes a cache useless: it was withheld from
the multis whose resolution is not cacheable, and therefore from precisely the
calls where re-resolving costs the most.

## Site 3 was re-deriving something it had been given

`push_multi_dispatch_frame` needs the winning candidate for two things — to
exclude it from the `remaining` list `nextsame`/`callsame` walk, and to read its
scalar `is rw` params — and it obtained it by resolving the name again.
`compile_and_call_function_def` is *handed* the resolved `FunctionDef` it is
about to call, so a new `push_multi_dispatch_frame_with_winner` takes it
directly. That is also strictly more accurate than resolving by name: the frame
now describes the candidate actually being invoked, whatever resolver picked it
(a user-defined operator, `MAIN`, a coercion, a trivial-proto dispatch).

Rakudo resolves a call once. So does mutsu now.

## Measured

Release builds on the same box, 60,000 calls of a two-candidate `multi` with a
`where` constraint (`tmp/bench-multi-where.raku`):

| | before | after |
| --- | --- | --- |
| wall clock | 12.5s | 5.1s |
| `function-full-resolve` per call | 3 | 1 |
| `where` evaluations for one call of the repro | 16 | 6 |

`bench-fib`, `bench-ctor`, `bench-class`, `poly-call`, `method-call` and
`bench-grammar-parse` are unchanged within noise.

The remaining `where` evaluations are a separate problem: the single resolution
still tests candidates that a narrowest-first walk would not reach, and the
winner's constraint is evaluated a second time while its parameters are bound.
#7885 addresses the first half; rakudo's "evaluate once, reuse the bind" is
still ahead of both.

## Pins

- `tests/multi_call_resolves_once.rs` — the `function-full-resolve` vm-stats
  count is 1 per call for a `where`-constrained and for a subset-constrained
  `multi`, and N for N calls in a loop.
- `t/routines/dispatch/multi-where-single-resolution-redispatch.t` — the
  redispatch chain still sees the right candidate list now that the frame is
  told its winner instead of re-deriving it: `nextsame`/`callsame` out of a
  `where`-constrained candidate, subset narrowness, and a user-defined operator
  `multi`. Every expectation was checked against rakudo v2026.07 first.
