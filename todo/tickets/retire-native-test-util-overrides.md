# Delete the now-dead native `Test::Util` / `Test::Tap` handlers

The override half of this is done: `user_test_decl_beats_native` consults the
wide `is_test_function_name` set, so a routine imported from roast's real
`Test::Util` / `Test::Tap` beats mutsu's native TAP provider, and all 228
whitelisted roast files that `use Test::Util` pass
(`news/2026-08/retired-native-test-util-overrides.md`).

What is left is the deletion. The native handlers now run only for a file that
calls one of those helpers **without** loading its module. Several `t/*.t` files
do exactly that today (e.g. `t/io-handle-lock.t`, `t/supply-list.t`), so
deleting the natives means adding the missing `use Test::Util` to them first —
worth doing on its own, since `raku` rejects those files as written.

Method:

```bash
# files that call a Test::Util helper but never load the module
grep -rl 'is_run\|is-path\|is-eqv\|group-of\|doesn.t-hang\|doesn.t-warn\|warns-like\|make-temp-' t/ \
  | while read -r f; do grep -q 'use Test::Util' "$f" || echo "$f"; done
```

Add the `use lib` + `use Test::Util` those files need, confirm each still passes
under both `mutsu` and `raku`, then remove the native handlers from
`src/runtime/test_functions/` and the corresponding names from
`Interpreter::is_test_function_name`. `t/test-fn-import-shadow.t` is the pin to
extend.

## Known residue, unrelated to the deletion

A file that ends with test failures prints `Runtime error: Test failures` on
stderr, which rakudo does not. mutsu's `run()` returns the failure as a
`RuntimeError` and `main` renders it. The exit status is already right (1), so
the fix is to set `exit_code` and return `Ok`, the way the bailed-out branch
does. Nothing asserts on it yet, but the next `is_run` slice will meet it.
