# ADR-0019 D3-8d: fix main-pass compilation for classes/roles nested in closures

D3-8d's original scope was a survey — sweep `t/` and roast with `MUTSU_VM_STATS=1` and confirm the
remaining `method_body_runtime_compiles` hits (registration-time throwaway compiles the main-pass
compiler, per D3-8a/b/c, should have made unnecessary) were all one of the already-enumerated
dynamic shapes (`augment class`, `.^add_method`, a computed class/role name). The sweep found
something bigger instead: a class or role declared inside **any** closure body — a `sub`, a bare
`{ ... }` block, `if`/`for`, an anonymous block passed to `subtest`, anything — unconditionally
skipped main-pass method-body compilation, not just the narrow "declared directly inside a
`subtest` block" case D3-8b's own regression fix was scoped to.

The root cause: every closure/sub body compiles under a synthetic STATE-SCOPE pseudo-package
(`current_package` containing `::&`, assigned unconditionally purely for `state`-variable key
uniqueness — unrelated to whether the body actually uses `state`). D3-8b's fix for
`roast/S12-introspection/walk.t` (a `$?PACKAGE.^name` returning a mangled name for a class inside a
`subtest` block) treated this pseudo-package as an unrecoverable case and bailed out of main-pass
compilation entirely whenever it was present. It is recoverable: `self.enclosing_package` — already
captured before the state-scope override, specifically so `$?PACKAGE` resolves to the real
declaring package, and already propagated unchanged through arbitrarily deep closure nesting — IS
the real runtime package the class/role will register under. A closure/sub body never itself
changes the interpreter's `current_package()`; only an explicit `class`/`package`/`module`/`unit`
bracketing does, and that always sets `current_package` directly to the real name (bypassing the
mangled form) regardless of nesting depth.

The fix: `qualified_class_decl_name`/`qualified_role_decl_name` now use `enclosing_package` as the
base package whenever `current_package` is state-scope-mangled, and the `in_state_scope` bail-out
is dropped entirely from `add_class_decl_plan`/`add_role_decl_plan` — the name predictors resolve
correctly either way now, so no special-casing is needed at those call sites.

Verification: the D3-8a byte-parity unit tests (11/11, plus new closure-nesting coverage), the full
`t/` suite (2974 files, 28019 tests), all 121 whitelisted `roast/S12-*`/`S14-*` files, and the
original `walk.t` regression pin all stayed green. A before/after `MUTSU_VM_STATS=1` sweep across
those 121 whitelisted files measured the fix directly: the summed `method_body_runtime_compiles`
count dropped from 494 to 330 (a 33% reduction), and 6 files — `walk.t` itself among them, 29 → 0 —
reached zero entirely.

The remaining ~330 hits are a second, distinct, already-documented cost: `subtest NAME => { ... }`,
called the common way (as an ordinary function taking a `Pair`, not through the dedicated
`Stmt::Subtest` statement-parser form), recompiles its block from AST on **every call**
(`eval_block_value`, the same EVAL-like re-entrant path used for `EVAL`) — confirmed via an
`rust-gdb` backtrace. Each such recompile re-triggers `hoist_type_decl_shells`, and the common
`plan N; class C {...}` test-file idiom places a runtime statement before the class, which is
exactly the shelling trigger — so the hoisted shell's method-body compile (already documented in
D3-8a/b as "otherwise-redundant" and deliberately left uncompiled, not a bug) fires on every single
`subtest` invocation. That is real but architecturally separate from method-body compilation, so it
is recorded as its own finding
(`todo/tickets/subtest-recompiles-block-from-ast-every-call.md`) rather than folded into this box.
