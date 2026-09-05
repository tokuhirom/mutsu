# `EVAL` of a RakuAST tree runs a `BEGIN` phaser at the wrong time

`BEGIN` runs at *compile* time, so a later mainline declaration overwrites
whatever it did:

```
$ mutsu -e 'my $x = 0; BEGIN { $x = 1 }; say $x'
0                                                    # correct, matches raku
$ mutsu -e 'use MONKEY-SEE-NO-EVAL; say EVAL(Q{my $x = 0; BEGIN { $x = 1 }; $x}.AST)'
1                                                    # raku: 0
```

Measured against rakudo 2026.07: raku answers `0` for both spellings, and so
does mutsu's own direct execution. Only the RakuAST `EVAL` carrier disagrees.

## Root cause

`builtin_eval`'s RakuAST branch lowers the tree and runs it through
`eval_block_value`. Since 2026-09-05 it also applies
`phasers::reorder_phasers_for_eval`, which fixes `CHECK` and `INIT` — those are
lifted out of statement position by `extract_phasers_from_stmts`. `BEGIN` is
*not*: that function takes a `_begin` accumulator it never fills, and
`run.rs`'s comment ("the hoisted BEGINs are gone before that pass") records
that BEGIN is hoisted earlier, during compilation of a program, by a mechanism
the re-entrant carrier never runs.

So a lowered `Stmt::Phaser { kind: Begin, .. }` executes in statement position,
i.e. *after* the `my $x = 0` that should have clobbered it.

## Current state

`src/rakuast/lower.rs` refuses `RakuAST::StatementPrefix::Phaser::Begin` rather
than lowering it to a wrong answer. The *read* direction is complete — `BEGIN`
renders as `RakuAST::StatementPrefix::Phaser::Begin`, byte-for-byte identical to
rakudo — so this is a write-direction-only gap, pinned as a boundary by
`t/rakuast-phaser.t`.

## Why it is not a small fix

The right fix is for the carrier path to run the same compile-time BEGIN
handling the ordinary pipeline does, which means locating that mechanism (it is
not in `phasers.rs`, which only reorders CHECK/INIT) and making it reusable from
`eval_block_value`. That is the same "the carrier is not the real pipeline"
shape as the other `eval_block_value` debts CLAUDE.md lists, so it likely wants
solving once for every carrier rather than for RakuAST alone.

## Minimal repro

```
mutsu -e 'use MONKEY-SEE-NO-EVAL; say EVAL(Q{my $x = 0; BEGIN { $x = 1 }; $x}.AST)'
# expected 0 (raku, and mutsu's own direct execution), got a boundary error
# today; before the boundary was added, got 1.
```
