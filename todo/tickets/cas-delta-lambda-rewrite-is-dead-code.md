# `cas`'s delta-lambda compile-time rewrite never fires (perf-only, not a correctness bug)

Found 2026-08-19 while investigating `todo/deep/inline-start-blocks-clobber-a-later-declared-variable.md`
(now resolved, moved to `news/`).

## The bug

`src/compiler/expr_call.rs` (~line 765-790) has a compile-time optimization: a 2-arg `cas $var, ...`
whose lambda body is a single `$v + delta` expression should rewrite directly to
`__mutsu_atomic_add_var` (skipping the general lambda-invoking `cas` path), matched against the pattern
`[Stmt::Expr(Expr::Binary { op: Plus, .. })]`.

This pattern **never matches** in practice: `--dump-ast` on `cas $n, -> $v { $v + 1 }` shows every
pointy-lambda body actually begins with a `Stmt::SetLine(_)` statement before the real expression
statement, so the single-statement match always fails and every `cas` call — including this "delta"
shape — falls through to the general path.

Confirmed via `rust-gdb`: breakpoints on both the delta-path call site (~line 779) and the general-path
call site (~line 802) of `note_atomic_env_sync_target`, for exactly this input shape, show only the
general-path breakpoint ever firing.

## Why this is not a correctness bug

`builtin_cas_var` (`src/runtime/builtins_atomic_cas.rs:103-139`) has its OWN runtime-side delta detection
that filters `SetLine` correctly and does dispatch to the fast `builtin_atomic_add_var` path at
execution time. So the only cost is a missed COMPILE-TIME optimization (presumably: avoiding
constructing/invoking a lambda value at runtime for this common shape) — the ticket that found this
verified the general path produces correct results throughout.

## Fix

Filter `Stmt::SetLine(_)` out of the lambda body before pattern-matching for the delta shape, mirroring
what `builtin_cas_var`'s runtime detection already does. When fixed, keep `counts_as_write: false`
(matching the general `cas` path's `note_atomic_env_sync_target` call) — do NOT pass `true` for the
delta-rewritten path only, since that would create per-call-site cross-thread-lane divergence for the
same variable (one call site's `cas` gets cell-promoted, another's doesn't) depending on which shape of
lambda body a caller happened to write.
