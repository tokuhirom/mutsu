# Scope block-local infix-op overrides to their declaring source file

Previously, a user-defined `infix:<op>` sub declared anywhere in a test script
(e.g. overriding `+` for a custom `NotANumber` type) was active globally —
including inside module code called from that script.  This corrupted
Test.rakumod's internal arithmetic: `proclaim` incremented its run-counter with
`$num_of_tests_run = $num_of_tests_run + 1`, and the test script's custom `+`
was intercepted instead of the native one, making the counter always return the
custom type rather than an integer.

The root cause was `user_declared_infix_ops`, a flat `HashSet<String>` whose
presence check fired regardless of which compilation unit was executing.
`user_infix_override()` also read `?FILE` from the interpreter env, which is
*inherited* through call frames: when Test.rakumod's `proclaim` ran (called from
a test script), `?FILE` still reflected the test script's path, not
Test.rakumod's.

## Fix

Converted `user_declared_infix_ops` from `HashSet<String>` to
`FxHashMap<String, Option<String>>` where the value is:

- `None` → exported / universally visible (visible in all executing files)
- `Some(path)` → active only when the currently-executing compiled function body
  comes from that source file

Added `executing_cf_source_file: Option<String>` to the Interpreter, updated
on every compiled-function entry/exit across all call paths (fast, light,
light-typed, named-inner, closure dispatch) and during module loading.
`user_infix_override()` now checks this field instead of `?FILE`.

Exported operators (via `import_module` or `sub EXPORT`) are forced to `None`
(universal) by using `insert(None)` rather than `or_insert(None)`, which would
have left the file-scoped `Some(path)` entry set at declaration time intact.

## Effect

- `roast/integration/advent2013-day10.t` under `MUTSU_REAL_TEST=1`: 44/44
  (was 12/44 — test counter was corrupted)
- `roast/S06-operator-overloading/` suite: 223/223 (unchanged, including
  `imported-subs.t` 20/20 with module-exported `multi infix:<+>`)
