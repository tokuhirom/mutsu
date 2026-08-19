# `start` blocks are forced onto the slow interpreter path, never OTF-compiled

Split out from `todo/deep/inline-start-blocks-clobber-a-later-declared-variable.md` (now resolved and
moved to `news/`) on 2026-08-19: this piece of that ticket was NOT fixed by the `needs_env_sync` /
`rw_arg_env_sync_syms` fix that resolved the clobber bug, and needs its own investigation.

## The gate

`expr_needs_interpreter` (`src/vm/vm_call_func_ops.rs:1994-2009`) unconditionally treats any
`Expr::Call { name: "start", .. }` as requiring the tree-walk/interpreter fallback rather than the
OTF-compiled path — regardless of whether the specific `start` block in question would actually be
unsafe to compile:

```rust
Expr::Call { name, args } => {
    // start blocks need the interpreter for proper thread spawning
    name.resolve() == "start" || args.iter().any(Self::expr_needs_interpreter)
}
```

## Why the exclusion exists

Regression pin: `t/start-block-return-value.t` test 3. The scenario is a recursive sub whose `start`
closure captures a parameter — the recursive call's re-bind of that parameter can clobber the closure's
capture before/while the spawned block reads it, if the block were OTF-compiled and its capture handled
by the compiled path's (different) capture machinery. See `news/2026-07.md` for the original proof of
infeasibility and investigation history.

## Why the clobber-bug fix doesn't help

The clobber-bug fix (see `news/2026-08/` — search for the `rw_arg_env_sync_syms` / Gap 4 entry) narrows
which locals need cross-thread name-lane reconciliation. It does not address the OTF gate's actual
concern, which is about **per-call capture identity** for a recursive sub's parameter — a `needs_env_sync`
style per-slot signal doesn't distinguish "this call's own capture" from "a sibling recursive call's
re-bound slot." The real fix, per the ticket that originated this note, is **per-call capture cells** (each
invocation of a recursive sub gets its own cell for a captured, re-bound parameter) or an equivalent sound
cross-thread capture-identity mechanism — a distinct, likely substantial design, not a corollary of the
`needs_env_sync` work.

## Next steps

1. Reproduce `t/start-block-return-value.t` test 3's failure mode with the OTF gate manually disabled
   (temporarily flip the `"start"` check off) to confirm the exact corruption shape under the current
   compiler/VM (the pin predates several capture-related refactors; re-verify it still reproduces the same
   way before designing around it).
2. Investigate whether per-call capture cells are already partially modeled by any existing mechanism
   (e.g. the `ContainerRef`/cell-promotion machinery touched by ADR-0013, or the box_captured_lexicals
   capture-at-spawn path) that could be extended, versus needing genuinely new bookkeeping.
3. Once a sound design exists, narrow `expr_needs_interpreter`'s `"start"` case from a blanket exclusion to
   only the specific unsafe shape (recursive sub + captured re-bound parameter), and verify
   `t/start-block-return-value.t` plus a broad `roast/S17-*` sweep.
