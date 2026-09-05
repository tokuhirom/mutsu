# RakuAST phasers

`BEGIN`, `CHECK`, `INIT`, `END`, `ENTER`, `LEAVE`, `KEEP`, `UNDO`, `FIRST`,
`NEXT`, `LAST`, `QUIT` and `CLOSE` now render, and all but `BEGIN` lower back
through `EVAL`. A phaser used to be a `.AST` coverage boundary printing mutsu's
internal `Stmt::Phaser` debug form.

raku gives each kind its own class,
`RakuAST::StatementPrefix::Phaser::<Kind>`, wrapping the block positionally.
mutsu's single `Stmt::Phaser { kind, .. }` maps onto them 1:1, so the whole
family lands in one slice. Measured against rakudo 2026.07: the rendered gists
are byte-for-byte identical for every kind checked.

`PRE`/`POST` stay a boundary on both sides. rakudo desugars them into a call
*around* the block — the phaser's child is an `ApplyPostfix`, not a `Block` —
and mutsu additionally keeps the condition's source text for the
`X::Phaser::PrePost` message, so neither the shape nor the extra field maps
cleanly.

## Two ordering bugs the oracle caught

Rendering was the easy half. Running the lowered tree revealed that mutsu's
re-entrant `EVAL` carrier ran the *compile-time* phasers in statement position:

```
$ mutsu -e 'my $x = 0; INIT { $x = 1 }; say $x'
0                                              # correct, matches raku
$ mutsu -e 'use MONKEY-SEE-NO-EVAL; say EVAL(Q{my $x = 0; INIT { $x = 1 }; $x}.AST)'
1                                              # raku: 0
```

`INIT` and `CHECK` run before the mainline, so the later `my $x = 0` should win.
The string-`EVAL` path already applies `phasers::reorder_phasers_for_eval` for
exactly this reason; the RakuAST branch of `builtin_eval` did not. It does now,
and both kinds agree with raku.

`BEGIN` is not fixed by that. `extract_phasers_from_stmts` takes a `_begin`
accumulator it never fills — `run.rs`'s own comment records that BEGIN is hoisted
earlier, during compilation of a program, by a mechanism the carrier never runs.
Rather than lower it to a wrong answer, `RakuAST::StatementPrefix::Phaser::Begin`
is refused, and the gap is filed as
`todo/tickets/rakuast-eval-begin-phaser.md`. Its *read* direction is complete.

## Coverage

`t/rakuast-phaser.t` (15 assertions) pins one class name per kind for eight
kinds, the positional block, five `EVAL` round trips covering entry/exit and
loop phasers, and the pre-mainline ordering of `INIT` and `CHECK`. It is a
dual-oracle test: it passes verbatim under both mutsu and rakudo 2026.07.
