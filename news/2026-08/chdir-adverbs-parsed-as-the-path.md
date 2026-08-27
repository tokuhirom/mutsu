# `chdir` now handles its `:d`/`:r`/`:w`/`:x` adverbs correctly

`chdir` and `indir` share almost identical bodies in
`src/runtime/builtins_io_dir.rs`. `indir`'s adverb handling was fixed
earlier; `chdir`'s was deliberately left, because it also performs a
best-effort real process `chdir(2)` and the interaction between that and a
skipped existence test needed its own measurement before touching the code.

## The two bugs

1. **The adverb was taken as the positional path.** `builtin_chdir` picked
   `args.first()` unconditionally as the target path, so `chdir :!d, $path`
   fed the `:!d` `Pair` itself (stringified to something like `"d\tFalse"`)
   into the path resolver instead of skipping past it to find `$path`. Fixed
   by scanning for the first non-`Pair` argument, exactly like
   `builtin_indir` already did.

2. **`:!d` did not skip the existence test.** `:d` (default `True`) is what
   requests the directory test, and rakudo folds existence into that same
   test: `chdir :!d, $nonexistent` succeeds in rakudo and returns an
   `IO::Path`, but mutsu tested `!absolute_target.exists()` unconditionally
   regardless of `:d`. Fixed the same way `indir` was: the existence check
   (and the `is_dir()` check) now only fire when `require_dir` is true.

## What was measured before implementing (the reason this was deferred)

The open question was whether rakudo's `chdir` does a real OS-level
`chdir(2)`, since if it did, a `:!d` chdir to a nonexistent directory would
need to *skip* the real syscall (which would fail) while still succeeding at
the Raku level.

Measured against rakudo v2026.06 (comparing `$*CWD` against
`"/proc/self/cwd".IO.resolve`, i.e. the real process cwd, before/after
various `chdir` calls):

| Scenario | `$*CWD` | Real process cwd (`/proc/self/cwd`) |
|---|---|---|
| `chdir` to an existing directory | updates to the target | **unchanged** |
| `chdir :!d` to a nonexistent path | updates to the target | unchanged |
| `chdir :!d` to an existing file (not a dir) | updates to the file path | unchanged |
| child process spawned via `run`/`shell` after a `chdir` | inherits `$*CWD` as its cwd | (irrelevant — child's own cwd, set explicitly) |

Rakudo's `chdir` **never issues a real `chdir(2)` syscall, even for an
existing, real directory** — `$*CWD` is a purely virtual dynamic variable.
File I/O and subprocess spawning resolve relative paths against `$*CWD`
explicitly rather than relying on the OS working directory; mutsu already
does the same for `run`/`shell` (`builtins_system_run.rs` resolves
`opts.cwd` from `$*CWD` when no explicit `:cwd` is given).

mutsu's `builtin_chdir` still makes a best-effort real
`std::env::set_current_dir()` call when the resolved target is a real
directory (pre-existing behavior, out of scope for this fix, guarded by
`canonical.is_dir()` and never treated as fatal on failure). That guard
already tolerates a nonexistent target correctly on its own: for
`chdir :!d, $nonexistent`, `fs::canonicalize` fails and falls back to the
unresolved absolute path, whose `.is_dir()` is `false`, so the real syscall
is simply never attempted — no additional code change was needed there.

## Other observations recorded but not in scope here

- `chdir` to a file (not a directory) without `:!d` returns a `Failure`
  wrapping `X::IO::Chdir` with `os-error => "does not exist"` /
  `"is not a directory"` depending on the case; mutsu's existing message
  wording differs cosmetically but the exception class already matched
  (`roast/S32-io/chdir.t` only asserts on the class via `fails-like`, not the
  message text), so the wording was left untouched to keep this change
  minimal.
- A distinct, pre-existing parser bug was found while writing the
  regression test: a chained colon-pair adverb group before a positional
  argument (e.g. `chdir :!d:r, $path` or generally
  `someListOp :a:b, $x, $y`) is misparsed as a two-element list (the call
  with only the adverbs, followed by the positional arguments as siblings)
  instead of a single call with three arguments. This is unrelated to
  `chdir`/`indir` specifically — it reproduces for any listop call — so it
  is filed separately as
  `todo/tickets/chained-colonpair-adverbs-before-positional-misparse.md`.
  The regression test for this fix uses the equivalent
  comma-separated adverb form (`chdir :!d, :r, $path`), which parses
  correctly.

## Test

`t/chdir-adverbs.t` covers: `:!d` before and after the positional path
against a nonexistent target; multiple adverbs; a plain `chdir` (no
adverbs) to a nonexistent path still failing with `X::IO::Chdir`; a
successful `chdir` to a real, existing directory; and restoring `$*CWD` to
its original value at the end so later tests in the same process are
unaffected. Verified to pass under both `raku` and `mutsu`.
