# Fixed a native stack-overflow crash in ordinary recursive subs building a trailing-comma array literal of their own parameter

A recursive sub as simple as this crashed mutsu with a process-terminating
Rust stack overflow (SIGABRT, exit 134), even at recursion depth 1:

```raku
sub rec(Int $n) {
    my @v = ($n,);
    if $n > 0 {
        rec($n - 1);
    }
}
rec(1);
say "done";
```

`raku` prints `done`; mutsu aborted with `thread 'mutsu-main' has overflowed
its stack`. This was filed as
`todo/deep/recursive-sub-trailing-comma-array-literal-of-own-param-stack-overflow.md`
after a prior investigation session narrowed the repro shape (recursion +
a `my @array = ($param,);` trailing-comma list literal of the routine's own
parameter) but stopped short of using a debugger to confirm the actual
mechanism.

## Root cause (confirmed with `rust-gdb`)

A `rust-gdb -batch` session on the minimal repro showed the "stack overflow"
was not an infinite loop inside one Rust function — it was genuine,
unbounded *Raku-level* recursion: `rec` really was calling itself forever,
because the parameter `$n` never actually decremented across calls, even
though each call's *own* `$n - 1` computation was correct.

The trailing-comma list literal `($n,)` compiles to `WrapVarRef` +
`MakeArray`, which (via `Interpreter::capture_var_cell_inner` in
`src/vm/vm_data_ops.rs`) boxes the parameter's local slot into a shared
`ContainerRef` cell — so the array element aliases `$n`'s own container, as
Raku's List-of-a-variable semantics require — and mirrors that cell into the
call's `env` overlay under the parameter's name.

The fast call paths (`call_compiled_function_positional_light` and its
siblings in `src/vm/vm_call_light.rs`) chain a callee's `env` as a *scoped
child* of the live caller `env`, rather than cloning/flattening it, for
performance. For a plain scalar parameter that is normally only read via its
local slot, the compiler skips mirroring the freshly-bound argument into
`env` (`needs_env_sync` false — the common case). `exec_get_local_op`'s
"lazy sync" check (`src/vm/vm_var_assign_local_get.rs`) — meant to notice a
container established elsewhere in the *same* call frame but not yet
reflected in the local slot — looked the parameter's name up via a full
parent-chain `Env::get`/`get_sym`. For a *recursive* call, that parent chain
is literally the caller's own live frame: the lookup fell through past the
callee's own (env-mirror-less) parameter binding and found the CALLER's own
same-named boxed `ContainerRef` instead, silently adopting it as the
callee's own local slot. The callee's freshly-bound `Int(0)` argument was
discarded in favor of the ancestor's shared cell (still holding `1`), so
`$n` never actually became `0` inside the recursion, and `if $n > 0`
recursed forever.

## Fix

Added `Env::overlay_get`/`overlay_get_sym` — lookups that read only the
current frame's own overlay, never the parent chain — and switched
`exec_get_local_op`'s lazy-sync check to use them. A function body runs
under a single env tier (nested blocks do not push their own `scoped_child`,
only whole-call boundaries do), so the *intended* same-call-frame
propagation case still works; an ancestor call frame's same-named container
can no longer be picked up.

Regression test: `t/recursive-sub-trailing-comma-array-param.t`. Several of
its subs are deliberately written without an enclosing `lives-ok { ... }`
block and without referencing any outer/captured variable — either one
disqualifies a call from the fast light-call path this bug lived in, so a
naively-wrapped test would silently stop exercising the actual bug. Verified
against an unmodified pre-fix build that the file's `rec(1)` alone still
aborts the process, and that the fixed build's output matches `raku` for
every sub in the file (including the mutation/non-trailing-comma/constant/
square-bracket sanity variants that never triggered the bug).
