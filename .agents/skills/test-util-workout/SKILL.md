---
name: test-util-workout
description: Pick one Test::Util function from roast's Test-Helpers package, write a t/ test for it, and fix the interpreter until it passes, ending in a merged PR. Use when asked for a "Test::Util workout" or to work through the Test::Util helper functions.
metadata:
  short-description: Land one Test::Util helper end-to-end
---

# Test::Util function workout

`Test::Util` is **not** part of the Raku core. Its functions (`is_run`, `doesn't-hang`,
`make-temp-dir`, `make-temp-file`, ...) are defined in
`roast/packages/Test-Helpers/lib/Test/Util.rakumod`, and a roast test only sees them after it
`use`s the module. Never implement one as a core builtin — always read that source first to
learn the expected behaviour.

## Workflow

1. Read `roast/packages/Test-Helpers/lib/Test/Util.rakumod` for the list of exported functions.
2. Check `t/` for the existing coverage (`test-util-*.t`, `is-run.t`, ...) to see which
   functions already have tests.
3. Pick **one** unimplemented or undertested function. Once chosen, do **not** switch to a
   different one mid-task.
4. Write `t/<function-name>.t` exercising it with several cases: basic usage, edge cases, and
   combined checks.
5. Run it: `timeout 30 target/debug/mutsu t/<function-name>.t`.
6. Fix the interpreter until it passes. When the spec is unclear, check with `raku -e '<code>'`
   (the `install-raku` skill gets `raku` if the container has none) and consult `raku-doc/`.
7. Run `make test` and `make roast` to check for regressions, then read the results out of
   `tmp/make-test.log` / `tmp/make-roast.log` rather than re-running.
8. Branch off `main`, commit, push, and open a PR per the repository's PR workflow in
   `CLAUDE.md`.
9. Enable auto-merge: `gh pr merge --auto --merge <pr-number>` — `--squash` is rejected by this
   repository.

## Rules

- The implementation lives in `src/runtime/test_functions.rs`, **not** as a builtin in
  `builtins/`.
- If making the function work needs a new language feature (e.g. `exit_code` support),
  implement the feature properly. No stubs, hardcoded outputs, or early returns.
- Always read the `.rakumod` source before implementing — the helper's real behaviour, not an
  assumed one, is what the roast tests depend on.
