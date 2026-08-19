# `cas`'s delta-lambda compile-time rewrite now actually fires

`src/compiler/expr_call.rs` has a compile-time optimization for the 2-arg `cas $var, ...`
form: when the lambda body is a single `$v + delta` (or `delta + $v`) expression, the
compiler rewrites the call directly to `__mutsu_atomic_add_var`, skipping the general
lambda-invoking `cas` path. This rewrite was pattern-matched against
`[Stmt::Expr(Expr::Binary { op: Plus, .. })]` — a single-statement lambda body — but every
pointy-lambda body actually begins with a `Stmt::SetLine(_)` statement inserted by pointy
block parsing before the real expression statement, so the single-statement match never
matched and every `cas` call, including this delta shape, silently fell through to the
general path.

This was not a correctness bug: `builtin_cas_var`
(`src/runtime/builtins_atomic_cas.rs`) has its own runtime-side delta detection that
already filters `SetLine` correctly and dispatches to the fast path at execution time. The
only cost was a missed compile-time optimization — constructing and invoking a lambda
value at runtime for a very common shape (`cas $n, -> $v { $v + 1 }`) instead of emitting a
direct two-argument builtin call.

The fix filters `Stmt::SetLine(_)` out of the lambda body before the delta-shape
pattern match, mirroring the filtering `builtin_cas_var`'s runtime-side detection already
does. Verified via `rust-gdb` breakpoints (per the project's debugging guidelines) that the
delta-path call site now fires for `cas $n, -> $v { $v + 1 }` and the general-path call
site no longer does for that shape.

One subtlety: the delta-rewritten path's `note_atomic_env_sync_target` call was changed
from `counts_as_write: true` to `counts_as_write: false`, matching the general `cas`
path's existing call. Since the delta shape and the general lambda-invoking `cas` path can
both target the same variable — differing only by incidental lambda-body shape at a given
call site — using `true` for the (previously dead) delta path would have created
per-call-site cross-thread-lane divergence: one call site's `cas` on a variable would get
cell-promoted for free-var/capture-classification purposes while another call site on the
*same* variable would not, purely because of how its lambda body happened to be written.
Keeping both paths at `false` avoids that.

Added a regression test to `t/cas.t` that runs two concurrent `cas` calls on the same
`atomicint` variable — one hitting the (now-fixed) delta fast path, one falling through to
the general path via a slightly different lambda shape — and asserts no lost updates.
