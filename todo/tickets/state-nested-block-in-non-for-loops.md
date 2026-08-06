# `state` in a nested bare block persists across non-`for` loop iterations

The last residual of the state-scalar ticket family (the plain-`=` loss,
the block-as-loop-body loss, and the type-constrained-scalar loss are all
fixed — see `news/2026-08/state-scalar-cell-storage.md`,
`news/2026-08/state-block-loop-body-accumulates.md`,
`news/2026-08/typed-state-scalars-accumulate.md`).

Raku re-clones a bare block nested in a loop body on every iteration, so
its `state` restarts:

```raku
my $c = 0;
while $c++ < 3 { { state $n = 0; $n++; say $n } }   # raku: 1 1 1
```

mutsu persists it (1 2 3) for `while`/`until`/C-style `loop`/`repeat`
bodies: the #5959 sole-source-block ResetStateLocals suppression
(`loop_body_is_sole_block`) cannot tell the statement-modifier form (the
block IS the loop body — state must persist) from a nested sole block,
because `Stmt::While` and the C-style/repeat statements carry no
`is_statement_modifier` flag. The `for` twin was fixed by gating on
`Stmt::For::is_statement_modifier`
(`news/2026-08/state-block-loop-body-accumulates.md`); `Stmt::While` has
~44 construction sites, so adding the flag (or a parser-side marker on the
body block) is its own slice. `do { ... } for @xs`
(`compile_collected_loop_body`) has the same conflation.
