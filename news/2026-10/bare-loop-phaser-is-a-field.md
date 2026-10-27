# A statement-form loop phaser is a field of `Stmt::Phaser`, not a synthetic block

`NEXT $n++;` / `FIRST ...;` / `LAST ...;` (a phaser written over a bare statement) used to be
parsed as a phaser whose body was `[SyntheticBlock([stmt])]`, a marker the compiler's
`expand_loop_phasers` and the phaser lifter pattern-matched, that `convert` unwrapped, and that
`lower` re-created. `Stmt::Phaser` now carries `bare: bool` (the body is the one statement), and the
marker, `ParsedStmt::BarePhaser`, `bare_prefix::lower_phaser_body` and the unwrapping in
`bare_phaser_statement` are deleted.
