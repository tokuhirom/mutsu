# A defaulted parameter costs a full by-name resolve on every call

A routine whose signature has a defaulted (or `?`-optional) positional
parameter is excluded from the cached positional light-call path, so every
single call to it re-resolves the callee by name through
`resolve_function_with_types`. A routine with the same arity and body but no
default resolves **once** for the whole program.

## The measurement

Debug build (`MUTSU_VM_STATS` counters are optimization-independent), 1000
calls in a loop, reading `function-full-resolve`:

| signature | call | `function-full-resolve` total |
| --- | --- | --- |
| `sub plainf($a) { $a }` | `plainf($_)` | **1** |
| `sub plainf($a, $b = 1, $c = 2) { $a }` | `plainf($_)` | **1001** |

Same body, same call site, same number of arguments passed. The only difference
is that two trailing parameters carry defaults.

## The gate

`Interpreter::is_positional_light_call_eligible` (`src/vm/vm_call_eligibility.rs`)
requires, per parameter:

```rust
&& pd.default.is_none()
&& !pd.optional_marker
```

and `call_compiled_function_positional_light_at` (`src/vm/vm_call_light.rs`)
relies on that: it binds `param_local_slots` positionally straight off the
stack and treats any arity mismatch as an error, because "every
positional-light-eligible parameter is a mandatory positional".

This is the same *shape* of gate that commit `a7373e323` lifted for `&`-sigil
parameters (`t/amp-param-positional-light.t` pins that one).

## Why it matters beyond one benchmark

Defaulted trailing parameters are ordinary Raku style, and they are the shape of
every assertion routine in the vendored upstream `Test.rakumod`:

```raku
multi sub ok(Mu $cond, $desc = '')                      { ... }
sub proclaim(Bool(Mu) $cond, $desc is copy, $unescaped-prefix = '') { ... }
```

`roast/S03-buf/write-int.t` runs ~93 000 assertions and is the last file still
exceeding the per-file budget under `MUTSU_REAL_TEST=1`
(`todo/deep/vendor-real-test-module.md`); `proclaim` alone accounted for 280 110
of that run's 565 212 by-name resolutions.

## What a fix has to handle

The light binder cannot simply relax the gate: when a call omits a defaulted
parameter, something has to produce the default's value, and a default is an
arbitrary expression that may reference earlier parameters or close over the
outer scope. Two tiers, in increasing order of work:

1. **Exact-arity calls only.** Allow defaults in the gate but keep the light
   path's "actual == declared" requirement, so a call that passes every
   parameter takes it and a short call falls through unchanged. Cheap and
   contained; it covers `ok(1, "desc")` but not `proclaim($cond, $desc)`.
2. **Constant defaults filled by the binder.** Have the compiler record, per
   parameter, the *constant* value of a literal default (`''`, `1`, `Nil`) —
   and the type object for a bare `?` — on `CompiledFunction`, then let the
   light binder fill missing tail slots from that table. This covers both
   `Test.rakumod` shapes. A non-constant default keeps the routine off the
   light path.

A third tier (compiling default expressions into a callee prologue so the light
path handles every default) is the general answer but is a compiler change, not
a binder one.

## Measurement protocol

Iterate on the **debug** build against `function-full-resolve` — the counters
are deterministic and identical to release. Confirm the wall-clock win on a
release build afterwards, and re-run both `MUTSU_REAL_TEST=1` sweeps
(`scripts/test-module-sweep.sh`, `scripts/roast-test-module-sweep.sh`), which
are not part of CI.
