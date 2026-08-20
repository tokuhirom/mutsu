# Closed: the "second `is_run` with `$*RAKU`/`use v6.x` never spawns its child" ticket does not reproduce in any merged state

`todo/deep/is-run-after-raku-read-swallows-child-spawn.md` (filed 2026-08-14)
reported that a `t/`-resident test file using the vendored `Test::Util` and
calling `is_run` two or more times — with at least one later call's code
string reading `$*RAKU` under an explicit non-default `use v6.x` — silently
failed the second and later calls without ever spawning the child process
(`strace` showed no second `execve`), while the identical file under `tmp/`
passed. The ticket attributed the surfacing (not the mechanism) to the lazy
magic-vars Slice 2 change (`$*DISTRO`/`$*PERL`/`$*RAKU`/`$*VM`/`$*KERNEL`,
PR #6419, merge `da12365be`), verified at the time by `git stash` bisection
on the discovering session's working tree.

## Re-verification (2026-08-20): not reproducible, including at the discovery commit

The repro was exercised on the same machine, in a directory-structure-identical
checkout, against **both** today's `main` (`f9a4b61be`) **and the exact merge
commit where the trigger change landed** (`da12365be`, rebuilt from source):

- the ticket's verbatim minimal `t/repro.t` (whose
  `use lib $*PROGRAM.parent(1).add("roast/packages/Test-Helpers")` does not
  actually resolve, so `Test::Util` loads via `resolve_module_path`'s
  automatic `{ancestor}/roast/packages/{top}-Helpers/lib` fallback — the
  duplicate-candidate path the ticket suspected), plus a corrected
  `parent(2)`-based `use lib` variant, plus a 6-call variant interleaving
  unrelated `is_run` calls with repeated `use v6.c`/`use v6.e` + `$*RAKU`
  calls;
- relative and absolute program-path invocation, direct and via `prove`;
- warm and cold precomp cache (`rm -rf ~/.cache/mutsu/precomp`);
- debug and release builds;
- 16 repeated runs at the discovery commit to rule out flakiness.

Every combination passes (all `ok`, correct child output `6.c`/`6.e`/`6.d`),
and `strace -f -e trace=execve` on today's `main` confirms one child `mutsu`
`execve` per `is_run` call — three children for the three-call file, exactly
as expected.

Since the failure does not reproduce even at the very commit whose merge the
ticket named as the trigger, no merged state of the repository ever contained
the bug as far as can be established now. The likeliest explanation is that
the original failure depended on unmerged working-tree state or environment
of the discovering session (a since-deleted agent worktree; the `git stash`
bisection there toggled Slice 2 on top of whatever else that tree held), not
on Slice 2 itself as merged. No fix was applied, and per policy no fake fix
was invented for an unreproducible symptom.

## What the close-out leaves behind

The ticket's stated reason for filing was that the trigger combination
(`t/`-resident caller, 2+ `is_run` calls, a later call referencing `$*RAKU`
under `use v6.x`) occurred nowhere in the `t/` suite, so a re-emergence would
sail past CI. That gap is now closed: **`t/is-run-raku-version.t`** is exactly
that combination (six `is_run` calls, unrelated calls interleaved with
repeated `use v6.c`/`use v6.e`/default `$*RAKU.version` reads, loaded from
`roast/packages/Test-Helpers` via the standard `$?FILE.IO.parent(2)` path)
and runs in CI with the rest of `t/`. If the fragility ever comes back in
merged code, that file catches it.

One reproduction-relevant observation worth recording: with a *relative*
program path (as `prove` passes it), `resolve_module_path`'s ancestors
fallback can never reach the repository root — `Path::ancestors` of
`t/file.t` ends at the empty path, which the loop skips — so the verbatim
repro's broken `use lib` makes `use Test::Util` fail outright under `prove`
unless invoked with an absolute path. Any future repro attempt for a
fallback-resolution bug must invoke `mutsu` with an absolute script path.
