# Retire the native `Test::Util` / `Test::Tap` overrides — the two original blockers are fixed, seven roast files stand in the way

`exec_call` lets an *imported* routine beat mutsu's native Test provider, but
only for the `Test` module's own export list (`runtime::TEST_MODULE_EXPORTS`).
The rest of `Interpreter::is_test_function_name` — roast's `Test::Util` and
`Test::Tap` helpers (`is_run`, `doesn't-hang`, `is-path`, `is-eqv`,
`is-deeply-junction`, `make-temp-file`, `make-temp-dir`, `make-temp-path`,
`group-of`, `doesn't-warn`, `warns-like`, …) — still dispatches to the native
implementation *even though the module is really loaded from source*
(`roast/packages/Test-Helpers/lib/Test/Util.rakumod`).

That override is a live rung-3 provider over a module mutsu can already parse
and load, so it should go the same way `Pod::To::Text` did.

## The flip is a one-line change; what gates it is the residue

```rust
// src/runtime/registration.rs, user_test_decl_beats_native
if !crate::runtime::is_test_module_export(name) {   // -> Self::is_test_function_name(name)
    return false;
}
```

Re-measure by flipping that line and running the 228 whitelisted roast files
that `use Test::Util`:

```bash
while read -r f; do grep -q "use Test::Util" "$f" && echo "$f"; done \
    < roast-whitelist.txt > tmp/testutil-files.txt
MUTSU_FUDGE=1 prove -j4 -e 'target/debug/mutsu' - < tmp/testutil-files.txt
```

## The two original blockers are fixed (2026-08-02)

1. ~~`IO::Path ~~ IO::Path` is always False~~ — the smartmatch arms matched the
   exact class name, so every `IO::Path::Unix` comparison (which is what
   `is-path`'s `cmp-ok $got.resolve, '~~', $exp.resolve` produces) fell through
   to the generic instance arm.
   `news/2026-08/io-path-accepts-is-inherited-by-its-spec-subclasses.md`.
2. ~~A `Proc::Async` output tap is only drained by `await`-ing *that*
   promise~~ — `doesn't-hang` awaits a `Promise.anyof` composite, whose result
   is a plain `True`, so the `Proc`-result replay hook never fired.
   `news/2026-08/composite-promise-replays-its-proc-taps.md`.

Two more general bugs were found by flipping the guard and measuring, and are
also fixed — they are why the count below is 7 rather than 9:

- a resumable warning raised by a native coercion unwound instead of being
  settled at the raise site, so `warns-like { Int.Numeric }` lost every
  statement after the call (`news/2026-08/a-warning-resumes-at-its-raise-site.md`);
- `bail-out` emitted "Bail out!" but exited 0 instead of 255
  (`news/2026-08/bail-out-exits-255.md`).

## Measured residue: 2 files, 2 causes (2026-08-02)

With the guard widened, 226 of the 228 files pass. Neither of the two is a
`Test::Util` incompatibility — each is a mutsu gap the native override was
hiding. Take them one at a time; the flip lands once they are both closed.

| files | cause |
| --- | --- |
| ~~`S24-testing/12-subtest-todo.t`~~ | **DONE** — the failure-diagnostic stream was chosen by nesting depth rather than by whether the failure was TODO'd, and every stderr diagnostic was emitted twice. `news/2026-08/tap-failure-diagnostics-pick-the-stream-rakudo-picks.md`. |
| ~~`S19-command-line-options/04-negation.t` (2, 3), `S19-command-line/arguments.t` (6)~~ | **DONE** — an unrecognised switch was taken for the program file, and an option-parsing error exited 1 where rakudo exits 0. Decision recorded in [ADR-0017](../../docs/adr/0017-cli-option-errors-follow-rakudo.md); `news/2026-08/cli-option-errors-follow-rakudo.md`. |
| ~~`S26-documentation/02-paragraph.t` (28)~~ | **DONE** — `--doc=Text` was not recognised (and `E<...>` was not decoded inside `=begin pod`). `news/2026-08/doc-equals-renderer-and-entities-in-begin-pod.md`. |
| `S03-operators/repeat.t` (56), `S16-io/words.t` (11) | one `warns-like` whose message does not match, and `words()` without arguments not reading `$*ARGFILES` — each an ordinary single-assertion gap. |

One difference is *not* in that list because nothing currently asserts on it,
but the next `is_run` slice will meet it: a file that ends with test failures
prints `Runtime error: Test failures` on stderr, which rakudo does not. mutsu's
`run()` returns the failure as a `RuntimeError` and `main` renders it. The exit
status is already right (1), so the fix is to set `exit_code` and return `Ok`,
the way the bailed-out branch now does.

## Then delete what is dead

Once the guard covers every `is_test_function_name`, the native `Test::Util` /
`Test::Tap` handlers only run for a file that calls those helpers *without*
loading the module. Several `t/*.t` files do exactly that today (e.g.
`t/io-handle-lock.t`, `t/supply-list.t`), so deleting the natives means adding
the missing `use Test::Util` to them first — worth doing, since raku would
reject those files as written. `t/test-fn-import-shadow.t` is the pin to extend.
