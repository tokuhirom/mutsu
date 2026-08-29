# `is copy` parameter stays aliased to an immutable Str when the argument arrives through a chain of sigilless `\`-captures

## Symptom

Under `MUTSU_REAL_TEST=1`, `roast/S32-num/rat.t`'s `eqv with zero-denominator
Rationals` subtest aborts the whole file with:

```
Cannot modify an immutable Str ( NaN Rat,     Inf Rat)
  in sub proclaim at .../modules/Rakudo-Core/lib/Test.rakumod line 621
  in sub is-deeply at .../modules/Rakudo-Core/lib/Test.rakumod line 621
  in sub e-no at roast/S32-num/rat.t line 508
```

This is unrelated to the infix-routine-form numeric-comparison bug fixed
alongside this ticket (see `news/2026-08/infix-routine-form-numeric-comparison.md`):
the `eqv` result itself is correct (`raku -e 'say (0/0).Rat eqv (70/0).Rat'`
and mutsu both answer `False`), so the test should simply pass (`ok`) — it
crashes on a passing case, before the `unless $cond` diagnostic-building path
in `proclaim` is even reached, so it is not about diagnostic-message
construction either.

## Minimal repro

```raku
use Test;

plan 1;

sub e-no (\r1, \r2, \desc) { is-deeply r1 eqv r2, False, desc }

subtest 'inner' => {
    plan 1;
    e-no <0/0>, <70/0>, ' NaN Rat,     Inf Rat';
}
```

`raku` prints:

```
1..1
# Subtest: inner
    1..1
    ok 1 -  NaN Rat,     Inf Rat
ok 1 - inner
```

`MUTSU_REAL_TEST=1 mutsu` (with the real vendored `modules/Rakudo-Core/lib/Test.rakumod`)
dies with `Cannot modify an immutable Str ( NaN Rat,     Inf Rat)` inside
`proclaim`.

## Root cause (traced, not fixed)

`modules/Rakudo-Core/lib/Test.rakumod`'s `proclaim` declares:

```raku
sub proclaim(
  Bool(Mu) $cond,
  $desc is copy,
  $unescaped-prefix = ''
) is hidden-from-backtrace {
    ...
    $desc = $desc ?? nqp::join(...) !! '';
    ...
}
```

`$desc is copy` should give `proclaim` its own writable copy of whatever was
passed, decoupled from the caller's container. The call chain here is:

1. `e-no (\r1, \r2, \desc) { ... }` — `desc` is a sigilless capture (`\desc`)
   bound directly to the caller's literal string `' NaN Rat,     Inf Rat'`
   (a `Str` constant).
2. `is-deeply(Mu $got, Mu $expected, $reason = '')` — `$reason` is an
   ordinary (not `is copy`, not `is rw`) parameter, so by spec it is a
   read-only alias to whatever was passed — here, the sigilless `desc`
   capture from step 1.
3. `proclaim($test, $reason)` — `$desc is copy` should snapshot a fresh,
   writable `Str` at this call boundary.

In mutsu, the assignment inside `proclaim` (`$desc = ...`) instead reaches
the *original* immutable string literal from step 1 — i.e. mutsu's `is copy`
parameter binding does not properly decouple from the source value when it
arrives through a chain of sigilless `\`-capture aliases (a capture parameter
binding directly to a caller variable/literal, then re-passed positionally
through an intermediate read-only `$`-sigil parameter).

This was NOT investigated further (no VM/compiler code read for the `is copy`
parameter-binding path) — this ticket is a traced-but-unfixed finding, not a
diagnosis of the exact compiler/VM site.

## Resolution (2026-08-30)

Fixed in `runtime/types/binding_signature.rs`. A sigilless capture stores
`__mutsu_sigilless_alias::*` and `__mutsu_sigilless_readonly::*` metadata in
the call environment. An `is copy` parameter begins a detached binding, so its
binder now removes inherited metadata for its own name before binding the
copied value. This prevents an assignment in the copy's body from following an
outer raw alias or inheriting that alias's immutable-literal marker.

Pin: `t/is-copy-sigilless-capture-chain.t`. The originally blocked
`roast/S32-num/rat.t` completes under `MUTSU_REAL_TEST=1`.

## Affected files (starting points for whoever picks this up)

- Wherever `is copy` parameter binding is compiled/executed (likely
  `compiler/` signature-binding code and/or `vm/vm_call_ops.rs` parameter
  materialization) — needs a `git grep -n '"copy"'` / `git grep -n 'is_copy'`
  sweep to find the exact binding site.
- Sigilless `\`-capture parameter handling (`indexed_varref_from_value` and
  related capture-alias plumbing referenced elsewhere in `runtime/`).

## Why this is a ticket, not a fix in this PR

It surfaced as a side effect of fixing `roast/S32-num/rat.t` test 749
(`±Inf/NaN ⇿ Rat`), which is the actual scope of the PR this ticket is filed
alongside. `rat.t` test 749 now passes under `MUTSU_REAL_TEST=1`, but the file
as a whole still aborts later at the point above — a genuinely separate,
pre-existing bug (reproduces identically on `main` before that PR's changes).
Fixing `is copy` parameter binding generally is a distinct, self-contained
unit of work.
