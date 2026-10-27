# RakuAST preserves bracketed cross and zip assignment

Bracketed list assignments such as `@a [Z=] 1, 2` and `@a [X=] 1, 2` now
retain Rakudo's `MetaInfix::Zip|Cross(Assignment(:item))` node in `.AST`. Plain
`Z=` and `X=` assignments use the same typed parser representation, so a `Z=`
initializer consumes its full comma-list right-hand side. Lowering continues to
use the existing assignment expansion. The focused regression checks both AST
shapes, EVAL, and the assignment results.
