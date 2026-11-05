# RakuAST::ArgList from-comma-list, unshift and invocant

`RakuAST::ArgList.from-comma-list($apply)` builds an argument list from the
operands of an `ApplyListInfix` (any infix; only the node class is checked,
as in rakudo, and anything else raises `X::TypeCheck::Binding::Parameter`).
`unshift` puts a node first, in place, and answers it like `push`. `invocant`
answers the `RakuAST::Expression` type object. All measured against rakudo
2026.09; `from-invocant-list`, `replace-args` and `set-arg-at-pos` remain.
