# A defaulted parameter no longer costs a full by-name resolve per call

`is_positional_light_call_eligible` rejected any signature with a defaulted or
`?`-optional positional, so a routine like

```raku
sub work($a, $b = 1, $c = 2) { $a + $b + $c }
```

could never enter `pos_light_call_cache` and re-resolved itself through
`resolve_function_with_types` on **every single call** — 1001 full resolves over
1000 calls, against exactly **1** for the same body with the defaults removed.
Even a call that supplied every parameter paid it.

## Constant fills, computed once

The gate was there for a real reason: an omitted parameter needs its default
evaluated, and a default is an arbitrary expression that the general binder
evaluates per call with the parameter shadowed by its own type object
(`eval_param_default`, which clones the AST and compiles it *afresh on every
call*). The light binder cannot do that.

But it does not have to for the common shapes. `src/opcode_param_fills.rs` now
derives two things from the signature at registration time:

* `param_const_fills` — the value each omitted parameter binds, when that value
  is a compile-time constant: an immutable scalar literal default (`''`, `1`,
  `True`, `0.5`) or the type object a bare `?` binds
  (`missing_optional_param_value`, a pure function). Verified once against the
  parameter's own declared constraint, so the bind loop can skip type-checking a
  filled slot entirely.
* `light_required_positionals` — the mandatory *prefix* length, and `None` when
  the signature cannot be served this way at all.

The eligibility check then admits a signature exactly when every optional
parameter reduced to a constant and the optionals form a suffix; the binder
compares the call against the prefix instead of demanding an exact arity, and
fills the tail from the table.

The rule is deliberately narrow, because several shapes look constant and are
not. A **container** literal (`$y = [1,2]`) is refused: the general binder
re-evaluates the expression per call and so hands every call its own container,
which a shared constant would not — `sub c($x, $y = [1,2]) { $y.push(3); $y.elems }`
must answer `3` every time, not `3 4 5`. A **non-constant** default (`$y = $x + 1`)
is refused because it may read an earlier parameter. A `where`, a trait
(`is copy`/`is rw`), a sub-signature, a type capture, a coercion type, and an
`@`/`%` optional (whose missing value is a *fresh* anonymous container) are all
refused too. Each of those keeps the whole routine on the general binder, exactly
as before.

## The marker leak this exposed

Admitting optionals surfaced a latent bug. The parser gives **every**
parenthesized zero-argument call a synthetic `__mutsu_test_callsite_line` Pair as
its sole argument (`identifier_call.rs`, so `?LINE` and deprecation reporting
have a line to quote). Every bind path strips it — except the positional light
one, which counted it as an argument. That was invisible only because a surplus
marker used to fail the arity check, and it was already wrong when the counts
happened to line up:

```raku
sub f($a) { $a }
f(1);              # warms the cache
say f().^name;     # main: Pair   now: "Too few positionals passed"
```

The strip now happens in the two callers rather than in the binder: the cached
dispatch already scans the arguments for the callsite line, so it knows whether
there is one to drop, and probing for it inside the binder instead measured
**+1.3% on `bench-tak`**. A `debug_assert!` in the binder states the precondition
so a future caller that forgets is caught by name.

## Result

Deterministic instruction counts (callgrind, release):

| benchmark | before | after |
| --- | --- | --- |
| `sub work($a, $b = 1, $c = 2)`, 300 000 calls | 31.67 G | **2.29 G** (13.8x) |
| `bench-fib` | 1 344.7 M | 1 349.8 M (+0.38%) |
| `bench-tak` | 1 639.7 M | 1 641.7 M (+0.12%) |
| `bench-ctor` | 1 747.5 M | 1 750.3 M (+0.16%) |
| `bench-class` | 1 452.2 M | 1 444.9 M (-0.50%) |

Wall clock on the defaulted-parameter loop: **3.44 s → 0.19 s** (min of 5
interleaved runs against a baseline binary built from `main`), which also takes
it past rakudo's 0.30 s on the same box. `function-full-resolve` for the 1000-call
repro goes 1001 → 1, for the omitted-argument call *and* the exact-arity one.

The residual few tenths of a percent on the all-mandatory benchmarks is the two
extra `CompiledFunction` fields, not a per-call decision: the arity check is
nested under one `actual_count != positional_count` test, so a call that supplies
every parameter never loads `light_required_positionals` at all.

## What is left

Tier 3 of the original ticket — compiling default *expressions* into a callee
prologue so every default, constant or not, reaches the light path — is a
compiler change rather than a binder one and is filed as
`todo/perf/non-constant-defaults-still-forfeit-the-light-path.md`.

This does **not** move the vendored `Test` module, and the ticket's claim that it
would is stale. `proclaim` carries `is copy` and a `Bool(Mu)` coercion; `ok` is a
`multi` (multi names are excluded from the name-keyed cache by construction) and
a test-assertion name (excluded again, separately). `roast/S03-buf/write-int.t`
under `MUTSU_REAL_TEST=1` reports the same 22 805 full resolves before and after
— the 565 212 the ticket quoted had already been cut by the multi-resolve-cache
fix (`news/2026-09/multi-resolve-cache-keys-carry-definedness-and-declared-type.md`).
