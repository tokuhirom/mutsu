# `PRE {}`/`POST {}` phasers are not enforced at the true top-level mainline

Discovered while implementing ADR-0048 Phase 2 (placeholder-scope rejection
for phaser bodies). Unrelated to that ADR — this is a separate, pre-existing
correctness gap in `PRE`/`POST` phaser support.

## Repro

```
$ raku -e 'PRE { False }; say "reached"'
Precondition '{ False }' failed
  in block <unit> at -e line 1

$ mutsu -e 'PRE { False }; say "reached"'
reached
```

`raku` enforces `PRE`/`POST` conditions wherever they appear, including
directly at the mainline (outside any `sub`/`method`). mutsu enforces them
correctly inside a routine body (`sub f { PRE { False }; 1 }; f()` does die
with the expected `Precondition '...' failed`), but a `PRE`/`POST` phaser
written directly at the top level of a script (or `-e` one-liner) is
silently accepted and never checked.

## Root cause (partial — not fully investigated)

`src/compiler/stmt.rs`'s `compile_pre_phasers`/`compile_post_phasers` are the
only place a `PRE`/`POST` body's `CheckPhaser` opcode is emitted. They are
called from exactly two places:

- `src/compiler/mod.rs`'s `compile_phaser_block_scope`, itself only invoked
  when `self.is_routine && Self::has_block_enter_leave_phasers(stmts)` (see
  `src/compiler/mod.rs` around line 3141) — i.e. only for a compiled
  **routine** body, never for the true mainline unit.
- `src/compiler/helpers_sub_body.rs` (sub/method body compilation, same
  routine-only gating).

The true mainline (`compile_unit` in `src/compiler/mod.rs`) never calls
`compile_phaser_block_scope`/`compile_pre_phasers`/`compile_post_phasers` for
its own top-level statement list, so a `Stmt::Phaser { kind: Pre | Post, .. }`
sitting directly in the mainline's statement list is presumably falling to
whatever no-op/pass-through arm handles an un-extracted phaser in
`compile_stmt` — the phaser is compiled as a statement, but its
`CheckPhaser` opcode (the actual condition assertion) is never emitted for
that path.

## Why this is separate from ADR-0048

ADR-0048 Phase 2 (see `docs/adr/0048-placeholder-scope-is-a-block-invocation-
contract.md`) added a placeholder-scope rejection check to
`compile_pre_phasers`/`compile_post_phasers` — correct wherever those
functions actually run, but that gave no coverage for `PRE`/`POST` at the
mainline precisely because the *whole* precondition-checking mechanism never
runs there, not just the placeholder check. `t/placeholder-scope-rejecting.t`
pins the sub-body form (which works today) and documents this gap in a
comment rather than testing the (currently broken) mainline form.

## Fix sketch (not attempted)

`compile_unit` would need to detect `PRE`/`POST` phasers in the top-level
statement list (mirroring `has_block_enter_leave_phasers`) and route the
mainline body through the same `compile_phaser_block_scope`-style extraction
`compile_phaser_block_scope` uses for routines — or a dedicated top-level
variant, since the mainline is not "a routine" in the same sense (no
`is_routine` compiler flag, no return-value plumbing to match). Needs a real
investigation into how much of `compile_phaser_block_scope`'s BlockScope
opcode framing is safe to reuse for the whole program vs. requires its own
narrower top-level-only wiring.

## Severity

Low: `PRE`/`POST` at the true mainline (outside any sub/method) is an
unusual, rarely-used pattern in real Raku code — most `PRE`/`POST` usage is
inside routine bodies, which already works correctly.
