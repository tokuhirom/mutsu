# `subtest NAME => { ... }` recompiles its block from AST on every call

## What

The common test-file idiom `subtest "name" => { ... }` does not compile its block through the
dedicated `Stmt::Subtest`/`OpCode::SubtestScope` bytecode path that exists in the parser/compiler
(`parser/stmt/simple/control_stmts.rs:subtest_stmt`, `compiler/stmt.rs:3802`). That parser form
only matches a bare `subtest NAME => { ... }` **statement**; the far more common Test-module usage
— `subtest` called as an ordinary function taking a `Pair` whose value is an anonymous block/sub —
resolves through `try_native_test_function` → `test_fn_subtest`
(`runtime/test_functions/tap_subtest.rs:133`) → `call_sub_value` → `eval_block_value` →
`compile_block_value_opts` → **a fresh `Compiler::compile()` call**, i.e. the same re-entrant,
EVAL-like compilation path used for `EVAL`/embedded regex `{...}` blocks — confirmed via an
`rust-gdb` backtrace during the ADR-0019 D3-8d survey (`todo/deep/adr0019-d3-8-method-body-main-pass-compilation.md`).

This means every single `subtest { ... }` call parses/compiles the block's AST from scratch, not
just once. For any class/role declared inside such a block preceded by a runtime statement (the
common `plan N; class C {...}` shape), this also re-triggers `hoist_type_decl_shells`'s
already-documented "shell always falls back to a runtime method-body compile" cost on every call —
which is why the ADR-0019 D3-8d sweep still found nonzero `method_body_runtime_compiles` hits
concentrated in `subtest`-heavy roast files even after fixing the closure-nesting bail-out gap.

## Why this is a separate finding

D3-8 is scoped to method-body compilation; this is a whole-block re-compilation cost one layer up,
orthogonal to it. Fixing it would mean either (a) making the dedicated `Stmt::Subtest` parser arm
match the common `subtest NAME => { ... }` function-call form too (routing it through the
already-compiled `SubtestScope` bytecode instead of `eval_block_value`), or (b) caching the
compiled block the first time a given source location's `subtest` call executes (similar to how a
loop body is compiled once, not per-iteration). Both are real compiler/runtime changes needing
their own investigation — not attempted here.

## Repro

```
use Test;
plan 1;
subtest "s" => {
    plan 1;
    class C {
        method m { 42 }
    }
    is C.new.m, 42, "ok";
}
```

Run with `MUTSU_VM_STATS=1`: `method_body_runtime_compiles` is nonzero (1) even though the same
class declared directly inside a plain `sub`/block (no `subtest`) compiles main-pass cleanly (0).
Wrapping the whole file's `subtest` block in a loop multiplies the recompile cost per iteration.

## Impact

Primarily compile-time/CPU overhead inside test files (which call `subtest` heavily) and the
bundled-battery test suites that use TAP-style nested subtests; not a correctness bug.
