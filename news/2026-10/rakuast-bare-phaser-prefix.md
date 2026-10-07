# RakuAST: `.AST` keeps the bare statement of a phaser

`BEGIN say 1`, `LEAVE say 1`, `FIRST say 1` and the other phasers written over a bare statement
now render as rakudo does, `StatementPrefix::Phaser::<Kind>(Statement::Expression(...))`, instead
of a phaser over the one-statement block. A spelling-keeping parse marks the form with a
`SourceForm::BarePhaser` record (ADR-12199 section 6.4); `lower` accepts either child, so
hand-built RakuAST round-trips. This is the last slice of #12199.
