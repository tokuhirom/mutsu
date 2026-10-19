# Named submethod declarations round-trip through RakuAST

Named `my`, `our`, and unscoped submethod declarations now convert to
`RakuAST::Submethod` without a scope field, matching Rakudo. Lowering restores
the existing non-inherited submethod marker, so EVAL keeps the declaration's
callable behavior.
