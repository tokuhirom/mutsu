# `PRE {}`/`POST {}` are now enforced at the true top-level mainline

`PRE {}`/`POST {}` phasers (precondition/postcondition assertions) were
correctly enforced inside a `sub`/`method` body, but were a silent no-op when
written directly at the true top-level mainline of a script (outside any
routine):

```
$ raku -e 'PRE { False }; say "reached"'
Precondition '{ False }' failed
  in block <unit> at -e line 1

$ mutsu -e 'PRE { False }; say "reached"'   # before the fix
reached
```

## Root cause

The bytecode compiler itself was never the problem: `compile_stmt`'s own
`Stmt::Phaser { kind: Pre | Post, .. }` arms (`src/compiler/stmt.rs`) already
compile a correct inline `CheckPhaser` assertion for a `PRE`/`POST` node
wherever it appears in a statement list — the same primitive that makes
routine-body `PRE`/`POST` work, and confirmed by `--dump-bytecode` emitting
the right `CheckPhaser` opcode for a mainline `PRE { False }` even before the
fix.

The actual bug was one step earlier, in `Interpreter::run()`
(`src/runtime/run.rs`). Before compiling the mainline body, `run()` calls
`split_block_phasers()` to pull `PRE`/`POST`/`ENTER`/`LEAVE`/`KEEP`/`UNDO`
phasers out of the top-level statement list (mirroring how these phasers are
extracted from a routine body). `ENTER` phasers were correctly spliced back
into `body_main` as real `Stmt::Phaser` nodes so the compiler and VM would
run them — but the `PRE`/`POST` results of that extraction were bound to
`_pre_ph`/`_post_ph` and never used again. They were extracted, then simply
discarded, so their `CheckPhaser` opcode was never reached at the true
mainline's real execution path (`dump_bytecode()` doesn't go through
`split_block_phasers()`, which is why the bytecode dump looked correct while
the actual `-e`/script run silently ignored the phaser).

## Fix

`run()` now re-splices `pre_ph`/`post_ph` back into `body_main` as
`Stmt::Phaser { kind: Pre | Post, .. }` nodes, the same way `enter_ph` was
already handled — no new compiler wiring needed, since `compile_stmt`'s
existing inline arm already does the right thing once the phaser reaches it.

Verified against real `raku`: a `PRE` runs before every other mainline
statement, even ones textually preceding it, and a `POST` runs after all of
them, even ones textually following it (both phasers act at the block
boundary, not at their textual position) — for example
`say "before"; PRE { False }; say "after"` never prints `before` in real
`raku`. So `pre_ph` is spliced to the very front of `body_main` (ahead of
`enter_ph`, matching the order `compile_phaser_block_scope` already uses for
routine bodies: PRE, then ENTER) and `post_ph` is appended to the very end.

`Interpreter::run()`'s own `tail_stmt_sinks_fresh_rvalue()` helper (used to
decide whether an unhandled `Failure` as the program's last statement should
throw) was adjusted to skip a trailing `Stmt::Phaser` node the same way it
already skipped trailing `SetLine` markers, so a mainline ending in
`<real last statement>; POST { ... }` still recognizes the real last
statement rather than treating the repositioned `POST` node as the tail.

## Tests

- `t/pre-post-phaser-mainline.t` — new regression test covering a failing and
  a passing `PRE`/`POST` at the true mainline (via `is_run`, exercising a
  real subprocess rather than `EVAL`), the "runs at the block boundary, not
  at its textual position" ordering, the statement-form (no braces) variants,
  and confirms routine-body `PRE`/`POST` is unaffected.
- `t/placeholder-scope-rejecting.t` — the row documenting the previous gap
  (`PRE { $^c }` at mainline used to be untestable because the whole
  precondition-checking mechanism never ran there) is now a real pinned test:
  a mainline `PRE {}` correctly rejects a placeholder parameter with
  `X::Placeholder::Block`, exactly like the sub-body form.
