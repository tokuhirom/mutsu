# The native `Test` TAP provider is deleted

mutsu shipped two TAP implementations. Since 2026-09-10 a bare `use Test` has
loaded rakudo's own `Test.rakumod` (vendored to `modules/Rakudo-Core/lib/`), and
the Rust one survived only behind `MUTSU_REAL_TEST=0`, as the escape hatch the
dual-provider sweeps drove. It is gone now, and so is the switch: there is one
`Test` under mutsu, and it is the module rakudo ships.

## The last comparison

The two providers can never be compared again, so both sweeps were run one final
time on `main` at `a3fb606`, release build, in the same container:

| sweep | pass under both | regressed under the real `Test` | passes only under the real `Test` | fail under both |
| --- | --- | --- | --- | --- |
| `scripts/test-module-sweep.sh` (3 950 `t/` files) | 3 926 | **0** | 1 | 23 |
| `scripts/roast-test-module-sweep.sh` (1 436 whitelisted roast files) | 1 432 | **0** | 0 | 4 |

Nothing the native provider could do was lost. The one `t/` file that passes
*only* under the vendored module is `supply-serialize-fifo.t`; the 23 and 4
"fail under both" rows are sweep artifacts rather than failures — the `t/` sweep
copies every file into one flat working directory, which breaks the files that
resolve fixtures by their own path (`use lib`, `$*PROGRAM.parent(2)`, the whole
`repo-*`/`use-dist-*`/`nativecall-*-in-module` family), and the roast sweep forces
`MUTSU_PRECOMP=0`, which is what `S10-packages/precompilation.t` measures. The
other three roast rows are the documented container-environment set
(`docs/agent-environments.md`): two `uid 0` vs `chmod` files and one sandboxed
network socket file.

Both sweep scripts are deleted with the provider they compared. Neither had
anything left to compare against, and a dual-provider sweep with one provider
silently measures the same half twice.

## What came out

| | lines |
| --- | --- |
| `src/runtime/test_functions/` (7 files) | 2 933 |
| `src/vm/vm_native_test.rs` | 83 |
| the subtest halves of `src/runtime/subtest.rs` | 153 |
| the two sweep scripts | 211 |

plus the machinery that existed only to keep two providers apart:

- `Interpreter::real_test_module_enabled()` and its seven gates. Each one becomes
  "the real module owns this": the `use Test` no-op in `runtime_module.rs`, the
  two dispatch entries (`calls.rs`'s `exec_call_sanitized` and
  `builtins_operators_fallback.rs`), the `&is-deeply`-style Routine synthesis in
  `accessors_resolve.rs`, `is_interpreter_handled_function`, the VM's
  `try_native_test_function`, and the fudge preprocessor's provider branch.
- `user_test_decl_beats_native`, whose only job was to let an imported
  declaration win over a native handler.
- `TEST_MODULE_EXPORTS` and `register_native_provider_exports("Test", …)`: the
  real module runs its own `is export` declarations, so nothing has to
  synthesize its `Test::EXPORT::DEFAULT` stash. `t/export-default-stash.t` now
  passes because `Test` reaches that the same way every source module always
  did, not because mutsu pre-populated it.
- The `__mutsu_backend_todo__:` marker in `run_roast_preprocess.rs` and its
  consumer in `test_ok_with_diag`. It asked the native provider to drop the
  `# TODO` annotation when a `#?rakudo todo` assertion actually passed; rakudo's
  own module has no such convention, so mutsu now reports a TODO pass the way
  rakudo does.
- `collect_eval_imported_function_names` and the whole EVAL imported-function
  preseed chain it fed (`set_eval_imported_function_preseed`,
  `EVAL_IMPORTED_FUNCTION_PRESEED`, the `imported_function_names` parameter on
  two parser entry points). Its only content was the `Test` export list, needed
  back when `use Test` registered no routines; `collect_eval_user_sub_names`
  collects the registry functions and `&name` code vars the real import leaves
  behind. Removing it surfaced a bug in that collector, which is fixed here: a
  multi candidate is keyed `Pkg::name/arity…`, and it took everything after the
  last `::`, so **every** imported multi reached the parse-time preseed as
  `is/2` rather than `is`. EVAL'd code calling one in listop form
  (`is [$sub()], [42], 'desc'`) therefore parsed its first argument as a
  subscript. That was masked for `Test` by the hardcoded list and is live for
  any other module's multis; `eval_q_bracket_statement_list_runs_declaration_then_assertion`
  is the pin.
- `Stmt::Subtest` / `OpCode::SubtestScope` / `exec_subtest_scope_op` — a
  dedicated statement form for `subtest NAME => { … }` that ran the native
  subtest machinery. It was already unreachable: `subtest_stmt` parses the name
  with `expression`, which consumes the `=>` itself, so the rule never matched
  and the form has been compiling to an ordinary `Call` (and, since the flip,
  running through the module's own `subtest`) all along.

## The TAP state collapsed with it

`tap_state.rs` held the native provider's bookkeeping. With the provider gone,
almost none of it has a producer any more, and the dead-code lint says so: the
plan counter (`planned`/`failed`/`next_ran`), the pending-`todo` ranges
(`TodoRange`, `force_todo`), the subtest stack (`subtest_depth`,
`subtest_callable_is_sub`, `subtest_todo`, `begin_subtest`/`end_subtest`) and the
bail-out flag are all things rakudo's `Test` keeps in its own Raku-level state.
Two things survive: `active()`, the "a test file is running" gate, and the shared
cross-thread counter, so an assertion run in a `start` block still numbers itself
against the main thread's counter.

That took three consumers with it, all of which had been asking a
permanently-zero `subtest_depth`:

- `OutputSink::emit`/`emit_stderr` lose their `subtest_active` parameter, and
  `Interpreter::subtest_active()` goes with it. A subtest's output is indented by
  the module's own output handles now, not by mutsu buffering it.
- `Thread.start` and the thread-clone path no longer suppress immediate stdout
  for "a thread spawned inside a subtest".
- `finish()`'s trailing TAP footer — the plan-mismatch line, the
  `# You failed N tests of M` summary and the bail-out exit status — is deleted.
  The vendored module emits all three from its own `END` phaser, and sets the
  exit status itself: verified, `plan 2; ok 1, "a"` still exits 255 with
  `# You planned 2 tests, but ran 1`, a failing assertion still exits 1, and
  `bail-out` still prints `Bail out!` and exits 255.

## What did NOT come out

`Test::Util` is a roast helper (`roast/packages/Test-Helpers/lib/Test/Util.rakumod`),
not part of this provider. `is_run`, `doesn't-hang`, `make-temp-dir` and friends
are untouched; retiring their native overrides is its own ticket.

`Interpreter::is_test_function_name` also stays, but it is a *recognition* list
now, not a dispatch table: the static known-routine tables
(`runtime/undeclared_routines.rs` and the ADR-0065 analysis frontend) use it to
avoid reporting `is-deeply` as an undeclared routine in a unit they never run,
and the bare-word resolver uses it to route a hyphenated zero-argument call
through function dispatch.

## The parse-time export list stays, for one measured reason

Deleting `TEST_MODULE_EXPORTS` also deleted the parser's fast path for
`use Test`, which sent `register_module_exports` down `find_and_scan_module` —
a full parse of the vendored module's 953 lines, once per process, on top of the
runtime's own (precompilation-cached) load. Measured on a release build,
`mutsu -e 'use Test; plan 1; ok 1, "x"'` went from **35 ms to 93 ms**. Every `t/`
and roast file pays that, so a parse-time list is kept — as
`parser::stmt::simple::module_exports::TEST_EXPORTS`, where it belongs, with a
comment that says it is a speed shortcut for a bundled module rather than a
native provider's export surface. Startup is back at parity (36 ms).

Unlike the constant it replaces, it cannot drift: `test_exports_match_the_vendored_module`
re-derives it from `Test.rakumod` on every `cargo test`. That already found three
names the old list was missing (`MONKEY-SEE-NO-EVAL`, `exit-ok`, `trait_mod:<is>`)
and two it invented (`force_todo` / `force-todo`, which rakudo's `Test` does not
export).
