# RakuAST::Block.bare-block

`RakuAST::Block.bare-block` answers what rakudo 2026.09 does: `True` for a
block that is a whole statement (`{ 2 };`, nested ones too), `False` for every
other block (a block used as a value, a statement's body, a phaser's block, a
hand-built block even inside a `Statement::Expression`). Rakudo keeps it as
parse-time state (`Block.new(:bare-block)` is rejected), so the statement
conversion records a hidden marker on the block, which `.raku`, `visit-children`
and the lowering do not see.
