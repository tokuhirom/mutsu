# RakuAST: declarator docs on anonymous subs and blocks

`my $f = #| doc anon sub {}` now converts to the `RakuAST::Sub` (or `Block`)
rakudo prints, with the trailing `.declarator-docs(leading => (...), trailing => (...))`
call, and lowering puts the text back into the code object's documentation slot,
so `.WHY` survives the `MUTSU_RAKUAST=1` round trip. Named declarations keep
their docs in the parser's side table and are not yet printed (a follow-up for
`.AST` text parity).
