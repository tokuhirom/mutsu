# `use fatal` no longer leaks out of sub bodies or closure value calls

Pragmas in Raku are lexically scoped: `use fatal` inside a `sub` or a
closure should not affect the caller after the call returns.

mutsu stores `use fatal` (and `use strict`, `use MONKEY-TYPING`) as
interpreter-wide flags (`Interpreter::fatal_mode` etc.). The bare-block
case was already correct via the `PushImportScope`/`PopImportScope`
opcode pair emitted around blocks that contain a `use` statement, but
three other shapes leaked:

| shape | status before fix |
|---|---|
| `sub foo { use fatal; ... }; foo()` | leaked |
| `do { use fatal; ... }` | already fixed (PushImportScope path) |
| `my $c = { use fatal; ... }; $c()` | leaked |

**PR #6123** added `save_pragma_state()` / `restore_pragma_state()` helpers
in `src/vm/vm_helpers.rs` and wrapped all named-function call entry points
in `src/vm/vm_call_func_ops.rs` with them.

**PR #6125** extended the fix to `call_compiled_closure` in
`src/vm/vm_closure_dispatch.rs`, which handles the `$c()` closure-value
case dispatched via the `CallOnValue` opcode.

A pinning test `t/use-fatal-pragma-scope.t` was added with 10 cases
covering all shapes.
