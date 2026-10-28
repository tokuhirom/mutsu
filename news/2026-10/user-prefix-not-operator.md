# A declared or imported `prefix:<not>` takes `not $x`

`not` is a loose-unary builtin in the parser, so a user `multi prefix:<not>(...)`
(declared or imported) was never reached: `not $x` always meant the builtin
`!`, while `!$x` already dispatched to `prefix:<!>`. When a `prefix:<not>` is in
scope, `not` now parses as that operator's call, with the builtin as the
fallback for argument types it does not accept. Red's
`multi prefix:<not>(Red::AST $a)` makes `not (C.a)` a `Red::AST::Not` node
(t/06-optimizer).
