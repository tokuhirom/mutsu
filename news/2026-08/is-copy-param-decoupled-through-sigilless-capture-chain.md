# An `is copy` parameter is decoupled even when its argument arrives through a chain of sigilless `\`-captures

Resolved 2026-08-30; moved out of `todo/tickets/` during the 2026-09-01 TRIAGE
regeneration (the file already carried a "Resolution" section).

## Symptom

Under `MUTSU_REAL_TEST=1`, `roast/S32-num/rat.t`'s `eqv with zero-denominator
Rationals` subtest aborted the whole file:

```
Cannot modify an immutable Str ( NaN Rat,     Inf Rat)
  in sub proclaim at .../modules/Rakudo-Core/lib/Test.rakumod line 621
  in sub is-deeply at .../modules/Rakudo-Core/lib/Test.rakumod line 621
  in sub e-no at roast/S32-num/rat.t line 508
```

The `eqv` result itself was correct; the crash was in the real `Test` module's
`proclaim($cond, $desc is copy, ...)`, whose `$desc = ...` reached the
*caller's* immutable string literal instead of a private copy. The argument had
travelled `e-no(\r1, \r2, \desc)` (sigilless capture of a literal) ->
`is-deeply(..., $reason)` (plain read-only parameter) -> `proclaim(..., $desc is
copy)`, and the copy never detached from the alias chain.

Minimal repro (raku passes `ok 1`; mutsu died inside `proclaim`):

```raku
use Test;
plan 1;
sub e-no (\r1, \r2, \desc) { is-deeply r1 eqv r2, False, desc }
subtest 'inner' => {
    plan 1;
    e-no <0/0>, <70/0>, ' NaN Rat,     Inf Rat';
}
```

## Fix

`runtime/types/binding_signature.rs`: a sigilless capture stores
`__mutsu_sigilless_alias::*` and `__mutsu_sigilless_readonly::*` metadata in
the call environment. An `is copy` parameter begins a detached binding, so its
binder now removes inherited metadata for its own name before binding the
copied value. An assignment in the copy's body can therefore neither follow an
outer raw alias nor inherit that alias's immutable-literal marker.

Pin: `t/is-copy-sigilless-capture-chain.t`. `roast/S32-num/rat.t` completes
under `MUTSU_REAL_TEST=1` (re-verified 2026-09-01: the repro above passes under
both providers).
