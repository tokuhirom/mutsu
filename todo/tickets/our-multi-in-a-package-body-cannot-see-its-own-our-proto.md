# An `our multi` in a package body cannot see its own `our proto`

A `module`/`package` body that declares an `our proto` and its `our multi`
candidates is rejected outright, even though raku accepts it:

```
$ raku  -e 'module M { our proto sub foo($) {*}; our multi sub foo(Int $x) { "M" } }; say M::foo(1);'
M

$ mutsu -e '<same>'
Cannot declare individual multi candidates in 'our' scope
```

Measured 2026-09-04 on `main` (adb18e12c) against `raku` v2026.06.

The same shape works everywhere else:

| shape | mutsu |
|---|---|
| mainline `our proto` + `our multi` | works |
| `class M { our proto …; our multi … }` | works |
| `module M { our proto …; multi … }` (my-scoped candidates) | works |
| `module M { our proto …; our multi … }` | **rejected** |
| `package M { our proto …; our multi … }` | **rejected** |
| `module M { proto …; our multi … }` | **rejected** |

## Root cause

`register_sub_decl_with_metadata` (`src/runtime/registration_sub.rs`, around the
`Cannot declare individual multi candidates in 'our' scope` message) rejects an
`our`-scoped `multi` unless `proto_subs` already holds
`{current_package}::{name}`. The check is skipped for a hoist pass, on the
documented reasoning that a hoisted candidate registers before the in-sequence
`RegisterProtoSub` runs and the in-sequence re-registration enforces the check
with the proto present.

A package body's candidates are installed by a *third* path that is neither the
hoist nor the in-sequence registration:
`Interpreter::preregister_inline_package_subs` (`src/runtime/run_prelude.rs`),
the CHECK-time prepass that makes an inline package's interface available during
compilation. It calls `register_compiled_sub_decl` directly, with
`current_package` set to the package, while the package body — and therefore its
`our proto` — has not run yet. `proto_subs` is empty for that key, so the check
fires. Confirmed with `rust-gdb` (breakpoint at the check; frame #2 is
`preregister_inline_package_subs::{closure#3}`).

## Why this is not a one-liner

Two fixes were prototyped and both are wrong as written:

1. **Mark the prepass registration `__hoisted`.** Makes the positive case pass,
   but *also* accepts `module M { our multi sub foo(Int $x) {…} }` with no proto
   at all, which raku rejects ("Cannot use 'our' with individual multi
   candidates"). The deferred in-sequence check never runs for a package-body
   sub: the prepass leaves a
   `__mutsu_inline_package_sub_preregistered::…` env marker and the body's
   registration recognises it and skips.
2. **Pre-register the package body's `proto` declarations in the prepass, before
   its candidates.** Correct in principle — it is the same "declaration-only
   interface" the prepass exists to publish — but it makes the *in-sequence*
   `register_proto_decl` see its own key already in `proto_subs` and answer
   `X::Redeclaration`. It therefore needs a prepass/in-sequence duplicate-proto
   protocol, the same shape as the existing
   `__mutsu_inline_package_sub_preregistered` marker but for protos, and it
   interacts with the `our`-scoped strictness of `register_proto_decl` (a
   nested/`our` proto no longer takes the lexical-shadow exemption, see
   `news/2026-09/our-proto-is-not-lexically-shadowable.md`).

That protocol is the work, and it belongs with ADR-0041 §6.4's "the routine
registry needs lexical scope" campaign rather than as a spot fix.

## Repro

The one-liners above. Related, already fixed:
`news/2026-09/package-body-multi-is-lexical-to-the-package.md` (the resolution
side of the same package boundary).
