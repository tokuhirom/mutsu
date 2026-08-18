# A `LEAVE` phaser directly inside an `if`/`given` block never fired at all

```raku
my $ran = 0;
if True {
    LEAVE $ran++;
}
say $ran;    # raku: 1   mutsu (before this fix): 0
```

Found while re-investigating
`todo/tickets/log-timeline-task-leave-phaser-process-reset.md`. That ticket
suspected a narrow, `Log::Timeline`/`PROCESS::`-specific write-through gap
("a `LEAVE`-phaser write not correctly re-registering in the caller chain").
Building a minimal, isolated repro instead found something much more basic
and general: `LEAVE` inside `if`/`given` didn't run *at all*, regardless of
whether it touched `PROCESS::` or a plain lexical.

## Two separate, narrow gaps

1. **`if`'s compile-time-constant-condition fold.** `if True { ... }` never
   emits the runtime condition check/jump at all (ADR-0006 §2.2 — the branch
   is selected once, at compile time). The ordinary (non-constant) `Stmt::If`
   arm in `stmt.rs` already checks `has_block_enter_leave_phasers(then_branch)`
   and routes through `compile_phaser_block_scope` (the same `OpCode::BlockScope`
   mechanism a bare `{}` block or a sub body correctly uses) when the branch
   has any of `ENTER`/`LEAVE`/`KEEP`/`UNDO`/`PRE`/`POST` — but the constant-fold
   shortcut, `compile_resolved_branch_body`
   (`src/compiler/helpers_control_flow.rs`), never did, despite its own doc
   comment claiming to "mirror" the ordinary arm.
2. **`given`'s body was compiled by iterating and compiling each statement
   in place.** An un-lowered `Stmt::Phaser { kind: Leave, .. }` alone
   compiles to a no-op (only the phaser-expansion machinery lowers it into
   real bytecode), so its `LEAVE` silently vanished regardless of the
   topic's constness.

Neither gap is `Log::Timeline`- or `PROCESS::`-specific: a plain `LEAVE
$counter++` inside either construct was equally silent, and the original
ticket's own "isolated LEAVE+PROCESS:: check" (a bare `sub`, no `if`/`given`)
had already correctly ruled out its own suspected root cause — the real
trigger was one level up, in the `if`/`given` wrapping.

## Fix

Both sites now check `has_block_enter_leave_phasers` and route through
`compile_phaser_block_scope`, mirroring the pattern the ordinary `Stmt::If`
arm already used. `ENTER` was unaffected (it already fired correctly through
a separate, simpler mechanism) and is pinned as a regression guard.

Regression tests: `t/leave-phaser-if-given-fires.t`.

## What remains open

A narrower residual: `for`/`while` loop bodies already correctly RUN their
`LEAVE` phaser (a plain side-effect counter fires), but a `LEAVE`-driven
write to a `PROCESS::` dynamic variable specifically doesn't propagate to
code after the loop exits — a different mechanism (`expand_loop_phasers`,
not `OpCode::BlockScope`) with its own, still-undiagnosed gap. Split off as
`todo/tickets/leave-phaser-process-write-lost-in-loop-body.md`.

Re-ran `Log::Timeline`'s own `t/logging.rakutest` against this fix: real,
measurable progress (9/30 passing before → tests 1-9 now pass, up from 1-6),
but 10-30 still fail — a deeper, separate gap in `Log::Timeline::Task`'s own
`.log`/`.start`/`.end` event-recording mechanics, not the LEAVE/`PROCESS::`
interaction this ticket targeted. Split off as
`todo/tickets/log-timeline-task-event-recording-empty.md`.
