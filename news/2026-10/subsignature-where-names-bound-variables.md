# A `where` in a destructuring sub-signature sees the variables the sub-signature binds

`Red::AST::Optimizer::AND` writes its candidates as
`GeGt $right (Red::AST :left($rl) where { compare $ll, $rl }, |)` after
`GeGt $left (Red::AST :left($ll), |)`. mutsu evaluated such a `where` before the rename
target `$rl` was bound, and without the earlier parameter's `$ll` during multi dispatch, so
the `where` read undefined variables and no candidate was ever selected (Red `t/06-optimizer`).
A rename target is now bound before its own `where` runs (both at bind time and while a multi
is matched), and a destructuring parameter's variables stay visible to the later parameters'
`where` clauses during dispatch.
