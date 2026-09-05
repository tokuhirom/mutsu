# Both EVAL carriers run `BEGIN` at compile time

`BEGIN` runs at *compile* time, so a mainline declaration textually after it
still wins, and a read textually before it still sees its side effects. mutsu's
own direct execution got this right; neither EVAL carrier did.

```
$ mutsu -e 'my $x = 0; BEGIN { $x = 1 }; say $x'
0                                                     # correct

$ mutsu -e 'use MONKEY-SEE-NO-EVAL; say EVAL(q{my $x = 0; BEGIN { $x = 1 }; $x})'
1                                                     # raku: 0

$ mutsu -e 'use MONKEY-SEE-NO-EVAL; say EVAL(Q{my $x = 0; BEGIN { $x = 1 }; $x}.AST)'
RakuAST: EVAL does not yet support lowering `RakuAST::StatementPrefix::Phaser::Begin`
```

## The ticket was about RakuAST; the bug was not

The ticket was filed against the RakuAST carrier, whose lowerer refused
`StatementPrefix::Phaser::Begin` rather than answer wrong — the read direction
was already byte-for-byte identical to rakudo, so it was a write-direction-only
boundary. Measuring it turned up the same defect one layer down: the **string**
`EVAL` path had no boundary to protect it and had been silently answering `1`.

The cause is the one the ticket predicted. `reorder_phasers_for_eval` fixes
`CHECK` and `INIT` because `extract_phasers_from_stmts` lifts those out of
statement position. `BEGIN` is not lifted there — it is handled *earlier*, by
`Interpreter::run_toplevel_begin_phasers`, a pass the mainline pipeline
(`run.rs`) calls before `reorder_phasers` and the re-entrant carriers never
called at all. So a lowered `Stmt::Phaser { kind: Begin, .. }` executed in
statement position, i.e. after the `my $x = 0` that should have clobbered it.

## The fix is the pass, not a second copy of it

`run_toplevel_begin_phasers` was already a reusable `&mut self` method taking
the statement list; it just had one caller. Both carriers now call it, in the
same order `run.rs` uses — BEGIN first, so the hoisted ones are gone before
`reorder_phasers_for_eval` buckets declarations, which is what keeps a
`my $c = @a.elems` initializer from being reordered ahead of a BEGIN that
populates `@a`.

Its deliberate narrowness carries over unchanged: only a *hoistable* BEGIN (no
declarations, barewords, or calls — anything that could resolve a symbol goes
through the ordinary path) is pre-run, its writes are rolled back and the phaser
left in place if it throws, and `drop_seeded_noinit_decls` keeps a seeded
variable's value from being reset by its own bare declaration. A BEGIN the pass
declines still runs where it stands.

With the carrier fixed, `src/rakuast/lower.rs` lowers
`StatementPrefix::Phaser::Begin` like every other kind and the boundary is gone.

## Scope

Still outside: `my $n = BEGIN { 5 }` in the *read* direction — a BEGIN
expression rather than a statement, which `convert.rs` refuses (`PhaserExpr`).
It is a boundary, not a wrong answer, and it is unrelated to the carrier.

Pinned by the new `t/eval-begin-phaser.t` (10 assertions across both spellings,
including the non-hoistable cases that must keep running in place) and by a new
row in `t/rakuast-phaser.t`, whose BEGIN paragraph was the ticket's boundary
note. Both pass identically under rakudo 2026.07 and mutsu.
