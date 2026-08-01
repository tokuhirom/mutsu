# An imported `ok`/`is`/`plan` beats mutsu's native Test provider

mutsu implements the `Test` module natively (`src/runtime/test_functions/`) and
intercepts `use Test`, so no `.rakumod` is ever loaded for it. The
statement-call path (`exec_call`) dispatched **every** name in
`is_test_function_name()` to those Rust routines *before* resolving user
routines, and without any gate at all — not even the
`loaded_modules.contains("Test")` check its sibling in
`builtins_operators_fallback.rs` applies. A module that exports its own `ok` was
therefore silently overruled.

It is a nasty failure to diagnose, because the two implementations then keep
separate counters. Loading rakudo's real `Test.rakumod` under an alias produced:

```
1..3
ok 1 - first     <- mutsu's native handler
ok 1 - like      <- the module's own routine, its own counter
ok 2 - third     <- the native handler again
```

which reads as a stale module lexical, not as two live implementations. The
module's own `proclaim` being entered exactly once is the tell.

The rule applied is the one from
`news/2026-07/qualified-call-no-longer-aliases-a-builtin.md`: decide on whether
a **declaration** exists, not on whether the name is a builtin. An imported or
user-declared routine that can accept the call's arguments now wins. `use Test`
registers no routines, so the ordinary path has nothing to compete with and is
unchanged — pinned both ways by `t/test-fn-import-shadow.t`.

The guard is scoped to the `Test` module's own export list, which moved to
`runtime::TEST_MODULE_EXPORTS` as the single copy (the parser and
`system_eval_string` each had their own identical literal; a name present in one
list and missing from another would now dispatch inconsistently, so one copy is
load-bearing rather than tidy). `is_test_function_name` is deliberately wider —
it also covers roast's `Test::Util` / `Test::Tap` helpers, which come from
modules that really are loaded from source. Widening the guard to those too was
measured (all 228 whitelisted roast files that `use Test::Util`) and needs two
unrelated fixes first; both are written up in
`todo/tickets/retire-native-test-util-overrides.md`.

This is a prerequisite for step 2 of `todo/tickets/vendor-real-test-module.md`:
exercising the genuine upstream `Test.rakumod` under a temporary alias only
means anything once the alias's routines actually run.
