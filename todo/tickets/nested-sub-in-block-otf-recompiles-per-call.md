# A `sub` declared inside a block is OTF-recompiled on every call when the block runs through `call_compiled_closure`

Ready for direct implementation: the root cause is identified down to the
call site, the repro is three lines, and the fix does not need a design pass.
This is also the named prerequisite for
[ADR-0055](../../docs/adr/0055-closure-free-vars-resolve-to-their-own-binding.md)
slice 4 — see "Why this blocks ADR-0055 slice 4" below.

## Repro

```raku
# tmp/nested.p6 -- sub declared INSIDE the block
my $blk = { sub foo () { 42 }; my $r; for ^2000000 { $r = foo }; $r };
say $blk.();
```

```raku
# tmp/outer.p6 -- identical, except the sub is at file scope
sub foo () { 42 }
my $blk = { my $r; for ^2000000 { $r = foo }; $r };
say $blk.();
```

Release build, `main` at `991b55ffa`: `nested.p6` takes **12.40s**, `outer.p6`
takes **3.83s**. Both report identical JIT stats
(`jit: compiles=2 entries=3999802 bailouts=0`) and both report
`interpreter_fallbacks=0`, so the 3.2x gap is invisible to every existing
`MUTSU_VM_STATS` counter.

## Root cause

`rust-gdb -batch` with a breakpoint on
`mutsu::runtime::Interpreter::compile_and_call_function_def`, run against a
3-iteration version of each script:

- `nested.p6` — the breakpoint fires **once per loop iteration**, with the
  backtrace `compile_and_call_function_def` ←
  `dispatch_func_call_inner` (`src/vm/vm_call_func_ops.rs:1413`) ←
  `exec_call_func_op` ← `exec_call_func_named_op` ← `exec_one_dispatch`.
- The same block invoked through the tree-walk carrier
  (`lives-ok { sub foo () { 42 }; ... }`) — the breakpoint **never fires**, and
  that version runs in 2.00s.

So: when the block runs through `call_compiled_closure` (which `.()` reaches
via `call_sub_value`'s `compiled_routine`/compiled-code machinery), the nested
`sub foo` is not present in the `CompiledFns` table that
`dispatch_func_call_inner` consults, so the earlier compiled-function fast-path
lookups (`vm_call_func_ops.rs:173`, `:460`, `:722`) all miss. Dispatch then
falls to the `user_function_matches_call` arm, resolves `foo` from the registry,
passes the OTF gate, and calls `compile_and_call_function_def` — **compiling the
sub's body from AST, on every call**.

The tree-walk carrier does not pay this because `eval_block_value`'s own compile
of the block body registers the nested sub into the *carrier chunk's*
`compiled_fns`, so the call hits the stable compiled-function path and reuses
one `CompiledCode`.

### Second, smaller cost: the `state` exclusion

When the nested sub also declares `state` — the shape
`roast/S04-declarations/state.t` uses,
`sub foo () {$ = 42}` (the anonymous `$` is a `state` variable) — the OTF gate
`Self::def_is_otf_compilable_module_single`
(`src/vm/vm_call_func_ops.rs:2167-2203`) rejects it on
`!Self::routine_body_facts(def).declares_state`, so it degrades one step further
to `record_function_fallback` + `vm_call_function_fallback`
(`vm_call_func_ops.rs:1417-1419`) — full tree-walk dispatch on every call.
That shape measures 14.81s with `interpreter_fallbacks=2000000 (50.0%)`, versus
5.04s through the carrier.

The `declares_state` exclusion exists because a per-call OTF recompile would
sever a *module* sub's shared `state` cell across threads (the comment at
`:2198-2200` says so, and `imported_state_body_for_def` is the path that admits
`state`). Fixing the primary cause above removes the need to relax it for this
shape: if the nested sub is in `compiled_fns` and never OTF-recompiled, the
`state` cell has nothing to sever.

## Full measurement table

2,000,000-iteration `for` loop calling `foo`, release build, `main` at
`991b55ffa`:

| nested `sub` | invoked via | `interpreter_fallbacks` | wall |
|---|---|---|---|
| `sub foo() { 42 }` inside the block | tree-walk carrier (`lives-ok`) | 0 | 2.00s |
| `sub foo() { 42 }` inside the block | `call_compiled_closure` (`.()`) | 0 | 12.40s |
| `sub foo() {$ = 42}` inside the block | tree-walk carrier (`lives-ok`) | 0 | 5.04s |
| `sub foo() {$ = 42}` inside the block | `call_compiled_closure` (`.()`) | 2,000,000 (50%) | 14.81s |
| `sub foo() {$ = 42}` at file scope | `call_compiled_closure` (`.()`) | 0 | 3.83s |

## Suggested fix direction

Make the closure's own compiled representation carry its block-lexically
declared subs, so `dispatch_func_call_inner`'s `compiled_fns` lookup hits:

1. At `MakeAnonSub` time (`exec_make_anon_sub_op`, `vm_register_ops.rs`), the
   block body is already compiled into `data.compiled_code` /
   `data.compiled_fns`. Check whether the nested `sub` is compiled into that
   `CompiledFns` at all — if the compiler emits it but the runtime consults a
   different table, the fix is plumbing, not compilation.
2. If it genuinely is not there, the block-body compile needs to register nested
   `sub` declarations into the closure's `CompiledFns` the same way the carrier
   compile does for its own chunk.

Either way the shape is per-closure-instance and stable, so it composes with the
`carrier_compile_cache` pattern rather than fighting it. Do **not** "fix" this
by relaxing `def_is_otf_compilable_module_single`'s `declares_state` gate — that
addresses only the smaller second cost and reintroduces the cross-thread
`state`-cell hazard the gate exists for.

## Why this blocks ADR-0055 slice 4

ADR-0055 slice 4 forks `call_sub_value`'s general `ValueView::Sub(data)` branch
onto `call_compiled_closure` whenever `data.compiled_code.is_some()`. Every
block that declares a helper `sub` would move from the fast column of the table
above to the slow one — which is exactly the "2.4x regression on
`roast/S04-declarations/state.t`" that made the earlier fork attempt get
reverted (see
`news/2026-08/eval-block-value-recompiles-every-call.md`). ADR-0055 §3 already
names this as a separate, still-open perf investigation that should land before
the fork is measured; this ticket is that investigation, now root-caused.

## Verification protocol

- `MUTSU_VM_STATS=1` on both repros: `interpreter_fallbacks` must stay 0 for the
  no-`state` shape and must **drop to 0** for the `state` shape.
- `rust-gdb -batch -ex 'break mutsu::runtime::Interpreter::compile_and_call_function_def' -ex run`
  on a 3-iteration version of `nested.p6`: the breakpoint must fire at most once,
  not once per iteration.
- Wall clock on both 2,000,000-iteration repros must approach the file-scope-sub
  row (3.83s), and the carrier rows must not regress.
- `make test` + CI roast for correctness. Note that CI does not catch a perf
  regression, so the numbers above are the actual safety net.
