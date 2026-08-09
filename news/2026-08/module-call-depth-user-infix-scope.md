# fix: user `sub infix:<op>` no longer leaks into compiled module code

In Raku, operator declarations are lexically scoped per compilation unit.
A `sub infix:<+>` declared in the test script must not intercept arithmetic
inside `Test.rakumod` (or any other compiled module), because the module was
compiled in its own lexical scope where the user's operator does not exist.

mutsu stored user-declared infix operators in a global `HashSet` on the
interpreter, giving them interpreter-wide (dynamic) scope instead of
lexical-per-compilation-unit scope.  When `advent2013-day10.t` declared
`sub infix:<+>`, `sub infix:</>`, and `sub infix:<&>` inside a block,
those declarations immediately hijacked all subsequent `+`, `/`, `&`
dispatches — including the counter arithmetic inside `Test.rakumod`'s
`is()` function.  As a result test numbers 28–44 appeared blank (the
counter returned `Nil` instead of an integer).

## Fix

Added `module_call_depth: u32` to `Interpreter`.  Every compiled-function
call path increments the counter on entry and decrements it on exit when
the called function belongs to an external module
(`CompiledFunction::source_file` is `Some`).  `user_infix_override` now
gates on `module_call_depth == 0`: while the interpreter is inside any
module frame, user-declared infix operators are invisible, restoring
lexical-per-compilation-unit semantics.

The counter is incremented/decremented in all four compiled-function
dispatch paths: `call_compiled_function_fast`,
`call_compiled_function_named`, `call_compiled_function_positional_light`,
and `call_compiled_function_light_spec`.

Also fixed `RoutineRegistrySnapshot` to include `user_declared_infix_ops`,
so that `BlockScope` blocks restore the operator set on exit — preventing
a block-local `sub infix:<op>` from leaking into subsequent code outside
the block.

## Affected test

`roast/integration/advent2013-day10.t` — all 44 tests now pass (was 27/44
or fewer depending on earlier operator declarations).
