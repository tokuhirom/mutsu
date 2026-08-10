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

## Deep-dive investigation (2026-08-10)

### Traced mechanism (the crux the ticket missed)

The block does **not** arrive as bare AST — it arrives as an already-compiled closure, and the
carrier throws the bytecode away:

1. `subtest "s" => { ... }` parses as a plain `Stmt::Call { name: subtest, args:
   [PositionalPair(Binary{op: FatArrow, ...})] }` (verified with `--dump-ast`). The dedicated
   `Stmt::Subtest` parser arm (`src/parser/stmt/simple/control_stmts.rs:373`, tried at
   `src/parser/stmt/mod.rs:181`) is effectively dead for this shape: it calls `expression(rest)`
   for the name, and `expression` greedily consumes the whole `"s" => { ... }` pair (FatArrow is
   an ordinary infix), so the arm's own `rest.starts_with("=>")` check at
   `control_stmts.rs:379` always fails and the parse falls through to the function-call form.
   The comma form `subtest "s", { ... }` is a plain two-arg call as well.
2. The main-pass compiler compiles the anon block into closure bytecode and emits
   `OpCode::MakeAnonSub` (`src/compiler/expr_closure.rs:113`). At runtime,
   `exec_make_anon_sub_op` (`src/vm/vm_register_ops.rs:194`) builds the `SubData` **with
   `compiled_code: Some(cc)` and `compiled_fns`** (lines 248-249) via `resolve_closure_code`.
3. The call dispatches through `try_native_test_function` (`src/vm/vm_call_dispatch.rs:93`,
   `src/vm/vm_native_test.rs:35`) → `test_fn_subtest`
   (`src/runtime/test_functions/tap_subtest.rs:91`), which runs the block via
   `self.call_sub_value(block, vec![], true)` at `tap_subtest.rs:133`.
4. `call_sub_value` (`src/runtime/resolution_call_sub.rs:119`) has a compiled fast path **only
   for `compiled_routine`** (line 383). A closure `SubData` carries `compiled_code`, never
   `compiled_routine` (see the comment at line 376), so it falls into the interpreter carrier:
   env clone/merge, then `self.eval_block_value(&data.body)` at line 718 →
   `eval_block_value_inner` → `compile_block_value_opts`
   (`src/runtime/resolution_eval.rs:99`, fresh `Compiler::new()` at line 104) → `run_nested`.
   The `compiled_code` sitting on the `SubData` is consulted only for metadata
   (`authoritative_free_vars`, supply marks) — never executed.

So the fix needs **no parser change and no new cache**: the compile result is already cached on
the `SubData` (keyed by code identity, env per-call — exactly the invalidation-free design a
cache would have to invent). The carrier just has to use it.

### Measured impact (debug build, 2026-08-10, main @ 52f217429)

- `tmp/subtest-bench.p6` — 200 `subtest "..." => { ... }` calls, 4 assertions each:
  **0.391 s**. Identical per-iteration work in a plain named sub called 200 times
  (`tmp/subtest-bench-baseline.p6`): **0.203 s**. ≈ 0.9 ms/call carrier+recompile overhead,
  ~2x file wall-clock for a subtest-shaped file.
- `rust-gdb -batch -ex 'break src/runtime/resolution_eval.rs:104' -ex 'ignore 1 100000' -ex run
  -ex 'info breakpoints' --args ./target/debug/mutsu ./tmp/subtest-bench.p6` →
  `compile_block_value_opts` hit **exactly 200 times** (one full re-compile per call).
- `tmp/subtest-class-loop.p6` — the ticket's class-in-subtest repro looped 50x:
  `MUTSU_VM_STATS=1` reports `method_body_runtime_compiles=50` (one per call). The identical
  shape invoked as a plain compiled closure (`tmp/class-block-loop.p6`, `my &b = { ... class D
  ... }; for ^50 { b() }`) reports **0** — proving the compiled dispatch path eliminates the
  per-call method-body recompile too.
- There is no dedicated counter for block re-compiles; `method_body_runtime_compiles`
  (`src/vm/vm_stats.rs:317`) only fires when the block declares a class after a runtime
  statement. Use the gdb hit count for the general case.

### Option analysis

**(a) Extend the `Stmt::Subtest` parser/compiler arm — REJECTED.**
- To fire at all, the arm would have to stop `expression()` before `=>` (a precedence-limited
  name parse) or destructure the parsed FatArrow `Binary` after the fact, plus grow matches for
  the comma form, block-first form, and `:todo`-style adverbs.
- More importantly, `OpCode::SubtestScope` (`src/compiler/stmt.rs:3849`,
  `src/vm/vm_scope_ops.rs:5`) is the *less correct* runtime: it inlines the body into the
  enclosing frame with none of the decl snapshot/rollback (`snapshot_subtest_decls` /
  `restore_subtest_decls`, `tap_subtest.rs:17-89`), env save/merge, or `plan skip-all`
  callable-kind handling that `test_fn_subtest` provides. Routing more shapes into it is a
  correctness downgrade, not just a perf change.
- Real-Test-module constraint: under `MUTSU_REAL_TEST=1`, `try_native_test_function` declines
  (`src/vm/vm_native_test.rs:49`) and the vendored `Test.rakumod`'s `sub subtest(&subtests,
  $desc)` handles the call — its `subtests()` invocation already dispatches the block value
  through the normal compiled call path. A parser arm that hard-routes `subtest` statements to
  `SubtestScope` would *bypass* the real module, fighting the project's rung-2 north star
  (eventually flipping the real module on for good, `src/runtime/runtime_module.rs:13`). So (a)
  could only ever help the native provider, and would have to be env-var-gated in the parser —
  ugly and wrong-direction. If anything, the near-dead `Stmt::Subtest` arm should eventually be
  retired in favor of the function-call form, not extended.

**(b) Compiled-block cache keyed by source location — REJECTED as redundant.** The compiled
block already exists on `SubData.compiled_code`, cached at closure creation, keyed by code
identity, with the captured env carried separately per closure instance. A second
location-keyed cache would duplicate that and add invalidation questions for nothing.

**(c) Compiled-first dispatch of the block value — CHOSEN.** This is exactly the PR #5942
lever (`reduce_call_step`, commit ecb5eba21, `src/runtime/builtins_reduce.rs:348`; extended to
`produce` in #5944/d08310b34): when the callable carries bytecode, route through
`vm_call_on_value` (`src/vm/vm_dispatch_helpers.rs:458`, already `pub(crate)` since #5942),
whose Sub-with-`compiled_code` fast path (line 531) runs `call_compiled_closure`
(`src/vm/vm_closure_dispatch.rs:108`) — the same well-tested path every ordinary `b()` call
takes, including free-var entry snapshot + changed-var writeback to the caller env
(`vm_closure_dispatch.rs` ~690-713). `vm_call_on_value` itself reroutes wrap-chained Subs back
to `call_sub_value` (line 499), so no wrap regression.

### Step-by-step implementation plan

All changes in `src/runtime/test_functions/tap_subtest.rs`. Do not touch the parser, compiler,
or VM.

1. Add a private helper on `impl Interpreter` in `tap_subtest.rs`, modeled 1:1 on
   `reduce_call_step` (`src/runtime/builtins_reduce.rs:348`):

   ```rust
   /// Run a subtest body callable. Compiled-first: a `Sub` carrying bytecode
   /// dispatches through the VM closure path; the `call_sub_value` AST
   /// carrier re-compiles `data.body` on every invocation (see the ticket
   /// header). Subs without bytecode keep the interpreter carrier.
   fn subtest_call_block(&mut self, block: &Value) -> Result<Value, RuntimeError> {
       let has_bytecode = matches!(
           block.view(),
           ValueView::Sub(d) if d.compiled_code.is_some() || d.compiled_routine.is_some()
       );
       if has_bytecode {
           self.vm_call_on_value(block.clone(), vec![], None)
       } else {
           self.call_sub_value(block.clone(), vec![], true)
       }
   }
   ```

2. Replace `self.call_sub_value(block, vec![], true)` at `tap_subtest.rs:133`
   (`test_fn_subtest`) with `self.subtest_call_block(&block)`.
3. Replace the identical call at `tap_subtest.rs:194` (`test_fn_group_of`) the same way.
4. Change nothing else in `test_fn_subtest`: the `callable_is_sub` detection (lines 123-127,
   runs before the call), the `Must give \`subtest\`` plan-skip-all error propagation (line
   137, dispatch-route-independent), the decl snapshot/restore (lines 132/145/159), and the
   post-call env merge loop (lines 148-158) all stay. After `vm_call_on_value` returns,
   `self.env` is the caller env plus the compiled path's free-var writebacks, so the merge loop
   sees the same mutated keys it does today.
5. `cargo fmt && cargo clippy -- -D warnings`.

### Verification / acceptance

- **Counter (primary signal):** `MUTSU_VM_STATS=1 timeout 30 target/debug/mutsu
  tmp/subtest-class-loop.p6` — `method_body_runtime_compiles` must drop **50 → 0** (the
  plain-closure control `tmp/class-block-loop.p6` already shows 0).
- **Recompile count:** the gdb breakpoint on `src/runtime/resolution_eval.rs:104` against
  `tmp/subtest-bench.p6` must drop **200 → 0** hits.
- **Wall-clock:** `tmp/subtest-bench.p6` should move from 0.391 s toward the 0.203 s baseline
  (debug build; exact number will retain some begin/finish_subtest overhead).
- **Behavior:** `prove -e target/debug/mutsu` on: `t/subtest-plan.t`,
  `t/subtest-reports-why-its-body-died.t`, `t/group-of.t`, `t/test-default-descriptions.t`,
  then the full set (`grep -rln subtest t/` → 47 files). Roast (with `MUTSU_FUDGE=1`):
  `roast/S24-testing/11-plan-skip-all-subtests.t` (exercises `plan skip-all` + `return` inside
  both Sub and Block callables — the riskiest semantics), `roast/S24-testing/12-subtest-todo.t`,
  and the rest of the whitelisted `S24-testing/` files. Then `make test` and let CI run full
  roast. No new `.t` needed — behavior must not change.

### Pitfalls

- **Topic propagation:** the interpreter carrier binds the caller's `$_` into a zero-arg bare
  block (`resolution_call_sub.rs:536-543`); the compiled path installs the topic by its own
  rules (`vm_closure_dispatch.rs` ~565-585). A subtest body reading the *outer* `$_` could
  observe a difference — but the compiled path is the language-standard `b()` semantics, so if
  a roast test disagrees it will fail deterministically; fix forward, don't gate.
- **Blocks without bytecode** (Subs built by interpreter-side code) keep the carrier via the
  helper's fallback — zero behavior change there.
- **MUTSU_REAL_TEST path** never reaches `tap_subtest.rs` (declined at
  `vm_native_test.rs:49`) — unaffected either way, but re-run one subtest-using t/ file with
  `MUTSU_REAL_TEST=1` as a sanity check.
- **Env-merge correctness:** if a subtest body's write to an outer lexical stops propagating,
  the suspect is the interaction between `call_compiled_closure`'s writeback and the manual
  merge loop at `tap_subtest.rs:148-158` (see the env-writeback campaign memory note for the
  bug shape); the loop only merges keys already present in `saved_env`, so a writeback that
  lands in a parent env tier rather than the overlay would be the thing to check.
- **Sibling recompile-per-call carriers** (same lever, out of scope here, worth follow-up
  tickets): `eval_exception.rs:477` (`lives-ok`/`dies-ok` code arg), `throws_like.rs:503`,
  `comparison.rs:135` (`cmp-ok` operator arg), `fails_like.rs:274`, and `classify`/`categorize`
  if still uncovered (produce/reduce were fixed in #5942/#5944).
