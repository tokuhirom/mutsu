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

## Measured residue: 7 files, 4 causes (2026-08-02)

With the guard widened, 221 of the 228 files pass. None of the seven is a
`Test::Util` incompatibility — each is a mutsu gap the native override was
hiding. Take them one at a time; the flip lands once they are all closed.

| files | cause |
| --- | --- |
| `S24-testing/12-subtest-todo.t` (test 5) | **A failing assertion's `# Failed test at …` diagnostic goes to stdout inside a subtest.** Rakudo splits it: a *TODO*'d failure's diagnostic goes to stdout (`$todo_output`), a real failure's to stderr (`$failure_output`), and a failing subtest also emits `# You failed N tests of M` to stderr. mutsu puts everything on stdout and omits the subtest summary, so the real `is_run`'s `:out`/`:err` predicates see the wrong split. Compare `raku -e 'use Test; plan 1; subtest "foos" => { todo 1; ok 0; ok 0 }'` with stdout and stderr separated. |
| `S19-command-line-options/04-negation.t` (2, 3), `S19-command-line/arguments.t` (6) | **CLI option handling**: mutsu exits 1 where raku exits with a different status for a malformed/negated short option, and writes an unknown-option warning to the wrong stream. |
| `S26-documentation/02-paragraph.t` (28) | **`--doc=Text` is not recognised**: mutsu treats it as the program file ("Could not open --doc=Text"). The real `is_run` passes it through `:compiler-args`. |
| `S03-operators/repeat.t` (56), `S16-io/words.t` (11) | one `warns-like` whose message does not match, and `words()` without arguments not reading `$*ARGFILES` — each an ordinary single-assertion gap. |

## Then delete what is dead

Once the guard covers every `is_test_function_name`, the native `Test::Util` /
`Test::Tap` handlers only run for a file that calls those helpers *without*
loading the module. Several `t/*.t` files do exactly that today (e.g.
`t/io-handle-lock.t`, `t/supply-list.t`), so deleting the natives means adding
the missing `use Test::Util` to them first — worth doing, since raku would
reject those files as written. `t/test-fn-import-shadow.t` is the pin to extend.
