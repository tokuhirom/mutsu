# A user-declared prefix/postfix operator keeps `-*.method` a WhateverCode

Once a program declared `multi prefix:<->` or `prefix:<!>` (Red does, in
`Red::Operators`), `-*.height` and `!*.closed` stopped currying: the operator is
parsed as a call of its `prefix:<op>` routine, an ordinary `Expr::Call`, whose
arguments never prime a WhateverCode, so `*.height` was evaluated on the `Whatever`
("No such method 'height' for invocant of type 'Whatever'"). Every
`sort(-*.col)` / `map(!*.col)` over a Red result seq died.

A call by an operator routine name (`prefix:<...>`, `postfix:<...>`) is now an
operator application for currying: `contains_whatever`, `count_whatever` and the
placeholder replacement read its operands through one accessor,
`operator_call_operands`. An ordinary call (`&infix:<+>(*, 42)`, `f(*)`) still hands
the callee a Whatever.
