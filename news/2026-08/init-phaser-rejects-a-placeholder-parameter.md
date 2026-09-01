# `INIT { $^c }` now raises `X::Placeholder::Block`

Closed 2026-08-29; moved out of `todo/tickets/` during the 2026-09-01 TRIAGE
regeneration (the file had carried its own "Closed" header for three days).

A phaser block is not a routine, so a placeholder parameter (`$^c`) inside one
is a compile-time `X::Placeholder::Block`. mutsu enforced that for every phaser
`t/placeholder-scope-rejecting.t` covers -- `BEGIN`, `CHECK`, `PRE`, `CATCH`,
`CONTROL`, `once`, `try`, `react`, `loop`, `default`, `gather`, `supply`,
`start`, `sink`, `lazy`, `module`, `package`, `grammar` -- except `INIT`:

```
$ mutsu -e 'try { EVAL q[INIT { $^c }] }; say $! ?? $!.^name !! "NONE"'
NONE                       # raku: X::Placeholder::Block
```

## Root cause and fix

Phaser reordering expanded a statement-level `INIT` body into ordinary
statements, losing the phaser context before compilation, so the existing
compiler guard never saw an `INIT` block. It now retains
`Stmt::Phaser { kind: Init, ... }`, and a tail-position `INIT` takes the same
guard through `compile_block_inline`.

Pinned by the direct-EVAL assertion in `t/placeholder-scope-rejecting.t`; that
28-test file passes under raku and under both mutsu `Test` providers
(re-verified 2026-09-01: `MUTSU_REAL_TEST=1` runs 28/28).
