# Delete the now-dead native `Test::Util` / `Test::Tap` handlers

The override half of this is done: `user_test_decl_beats_native` consults the
wide `is_test_function_name` set, so a routine imported from roast's real
`Test::Util` / `Test::Tap` beats mutsu's native TAP provider, and all 228
whitelisted roast files that `use Test::Util` pass
(`news/2026-08/retired-native-test-util-overrides.md`).

## Progress (2026-08-10)

The file-migration half is done: of the 21 `t/*.t` files the ticket's grep
flagged, only 10 were real (un-migrated) callers — the other 11 hits were
comment-only mentions of the function names. Added `use lib
$*PROGRAM.parent(2).add("roast/packages/Test-Helpers/lib"); use Test::Util;`
to 8 of those 10 (all still pass under both `mutsu` and `raku`):
`t/package-block-lexical-capture.t`, `t/supply-list.t`, `t/supply-collate.t`,
`t/type-object-numeric-coercion.t`, `t/io-path-secure-resolve-link.t`,
`t/pairup.t`, `t/nil-semantics.t`, `t/io-handle-lock.t`.

**2 files were reverted** after migration surfaced a genuine interpreter bug,
not a mechanical gap: `t/any-type-object-int-coercion.t` and
`t/bound-nil-method-warn.t` call `warns-like` on `Nil.Real`/`.Int`/`.Str`
coercions, and under the *real* `Test::Util`'s `warns-like` (which catches
the warning via `CONTROL { when CX::Warn { ... .resume } }`), the warning is
never observed at all — even though mutsu's native `warns-like` fallback (and
real `raku`, with the same import) both see it fine. Filed as
`todo/tickets/nil-method-warnings-are-not-a-resumable-cx-warn.md`. These two
files are intentionally left un-migrated (still using mutsu's native
`warns-like`) until that bug is fixed.

Verified with the ticket's own grep re-run after these edits: the ONLY
remaining un-migrated real caller in `t/` is `warns-like` (via the two
reverted files above); every other candidate name (`is_run`,
`Test::Util::run`, `get_out`, `is-eqv`, `group-of`, `doesn't-hang`,
`doesn't-warn`, `make-temp-file`, `make-temp-path`, `make-temp-dir`,
`is-deeply-junction`, `is-path`, `throws-like-any`) now has zero un-migrated
`t/` callers. Also checked all whitelisted `roast/*.t` files for the same
names without `use Test::Util` — none found (the only hit was
`roast/fudge`, a Perl helper script, not a test file).

## What is left: the deletion

`call_test_function`'s match (`src/runtime/test_functions/mod.rs:261-315`)
and `Interpreter::is_test_function_name` (`mod.rs:194-243`) can drop every
arm/name **except** `"warns-like"` (still needed — see above) and the core
`TEST_MODULE_EXPORTS` names (never touch those; they are `use Test` itself,
not `Test::Util`). Concretely, remove these match arms and their
`is_test_function_name` entries:

```
"throws-like-any" | "is_run" | "Test::Util::run" | "get_out"
| "doesn't-warn" | "is-eqv" | "group-of" | "doesn't-hang"
| "make-temp-file" | "make-temp-path" | "make-temp-dir"
| "is-deeply-junction" | "is-path"
```

Then delete the now-dead implementations (verify each has no other caller
before deleting — `grep -rn 'fn <name>'` and `grep -rn '\.{name}\('` first):

- **`src/runtime/test_functions/subprocess.rs`** (444 lines) — likely
  deletable **in its entirety**: `test_fn_is_run`, `test_fn_get_out`,
  `test_fn_run`, and their shared helpers `run_test_code_subprocess`,
  `is_run_subprocess`, `extract_run_output_with_source`,
  `split_tap_output_streams` appear to exist only for these three retired
  functions. Confirm no other module calls the helpers before deleting the
  file, then remove `mod subprocess;` from `test_functions/mod.rs`.
- **`src/runtime/test_functions/comparison.rs`** (580 lines) — remove
  `test_fn_is_deeply_junction` (~line 264) and `test_fn_is_eqv` (~line 558).
  This file also hosts core `Test` comparisons (`is-deeply`, `is-approx`,
  ...) that must NOT be touched — only delete the two named functions.
- **`src/runtime/test_functions/util.rs`** (274 lines) — remove
  `test_fn_doesnt_hang` (~line 8), `test_fn_make_temp_file` (~line 144),
  `test_fn_make_temp_dir` (~line 193), `test_fn_is_path` (~line 240). Check
  what (if anything) remains in the file afterward; it may become empty
  enough to delete along with its `mod util;` line.
- **`src/runtime/test_functions/tap_subtest.rs`** (200 lines) — remove
  `test_fn_group_of` (~line 164); the rest of the file is core `subtest`
  machinery, keep it.
- **`src/runtime/test_functions/throws_like.rs`** (742 lines) — remove
  `test_fn_throws_like_any` (~line 529); the rest is core `throws-like` /
  `fails-like`, keep it.
- **`src/runtime/test_functions/eval_exception.rs`** (568 lines) — remove
  `test_fn_doesnt_warn` (~line 451); the rest is core eval-exception
  machinery (`eval-lives-ok`, `eval-dies-ok`), keep it.

After deleting, extend `t/test-fn-import-shadow.t` per the original plan,
`cargo build` to catch anything still referencing the removed functions
(e.g. dead `use` imports flagged by clippy), then run the full `t/` +
`make roast` before opening the PR — this touches shared dispatch machinery
(`call_test_function`, `is_test_function_name`) used by every `t/`/roast file
that loads `Test`, so the safety net matters here more than usual.

## Known residue, unrelated to the deletion

A file that ends with test failures prints `Runtime error: Test failures` on
stderr, which rakudo does not. mutsu's `run()` returns the failure as a
`RuntimeError` and `main` renders it. The exit status is already right (1),
so the fix is to set `exit_code` and return `Ok`, the way the bailed-out
branch does. Nothing asserts on it yet, but the next `is_run` slice will meet
it.
