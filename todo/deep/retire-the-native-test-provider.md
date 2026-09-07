# Retire the native `Test` TAP provider

The vendored upstream `Test.rakumod` became the default provider on 2026-09-07
(`news/2026-09/vendored-test-module-is-the-default-provider.md`). mutsu now
ships **two** TAP implementations, and the native one is reachable only through
`MUTSU_REAL_TEST=0`. That switch is a transition aid so the dual-provider sweeps
can still compare the two — it is not a supported configuration, and keeping two
production providers indefinitely is exactly what the vendoring campaign existed
to end.

## What comes out

| | lines |
| --- | --- |
| `src/runtime/test_functions/` (7 files) | ~2 400 |
| `src/runtime/subtest.rs` | 700 |
| `src/vm/vm_native_test.rs` | 83 |

plus:

- `Interpreter::real_test_module_enabled()` (`src/runtime/runtime_module.rs`)
  and its seven call sites — `runtime_module.rs` (the `use Test` no-op and the
  `register_native_provider_exports` call), `builtins_operators_fallback.rs`,
  `calls.rs`, `accessors_resolve.rs`, `vm_call_dispatch.rs`,
  `vm_native_test.rs`. Each one becomes "the real module owns this", i.e. the
  branch it guards simply goes away.
- `TEST_MODULE_EXPORTS` and `register_native_provider_exports("Test", ...)`:
  the real module runs its own `is export` declarations, so nothing has to
  synthesize its export stash.
- The `__mutsu_backend_todo__:` marker in `run_roast_preprocess.rs` and its
  consumer in `call_helpers.rs` (`test_ok_with_diag`). Only the native provider
  ever understood it; the vendored path already emits the bare reason.
- `scripts/test-module-sweep.sh` and `scripts/roast-test-module-sweep.sh` —
  both are dual-provider comparisons with nothing left to compare against.
  Delete them with the switch, in the same change, so neither is left silently
  measuring one provider twice.
- `t/vendored-real-test-module.t`'s native half (subtests 3 and 4) and its
  `%*ENV<MUTSU_REAL_TEST>` steering. The file should keep asserting that the
  vendored module is what answers, without a second implementation to contrast
  with.
- `user_test_decl_beats_native` (`runtime/calls.rs`) and the second dispatch
  path it guards in `builtins_operators_fallback.rs`: both exist to let an
  imported declaration win over a native handler that will no longer exist.

## What must NOT come out with it

`Test::Util` is a **roast helper**, loaded from
`roast/packages/Test-Helpers/lib/Test/Util.rakumod`, not part of this provider.
Its own native overrides are a separate retirement, tracked by
`todo/tickets/retire-native-test-util-overrides.md`. `is_run`,
`doesn't-hang`, `make-temp-dir` and friends stay where they are.

## Why this is a `deep/` item and not a ticket

The seven gates are not independent: `calls.rs` and
`builtins_operators_fallback.rs` are two different dispatch entries into the
same handlers, `accessors_resolve.rs` synthesizes `&is-deeply`-style Routine
values that only exist because the natives are not declared subs, and
`runtime_module.rs` decides whether `use Test` is a module load at all. Removing
one at a time leaves the provider half-reachable in states nothing tests. It
wants one change, validated by `make test` + `make roast` on the default
provider, with `cargo test --lib` for the in-process TAP unit tests that today
assert native output shapes (`run_dist.rs`'s `rakudo_todo_*`, `runtime/mod.rs`'s
`test_more_tests_arg_emits_plan`) — those assertions collapse to the single
remaining provider's output.

## Before starting

Run both sweeps one last time on the current `main` and record the result in
the retirement's news entry: it is the last moment the two providers can be
compared, and the numbers are what justify deleting the loser.
