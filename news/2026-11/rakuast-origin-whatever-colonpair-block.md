# RakuAST: curried terms keep their own origin; `:k{...}` values

A curried term such as `*.a` lost its `.origin` to a later term of the same
shape in its statement: in `@a.map(*.a, *.a > 1)` both `*.a` reported the
second one's range, and the `ArgList` around them had none. The parser's range
was kept on the `WhateverCode` marker, which the view never reads; it is now
kept on the term the marker wraps.

`:k{ ... }` was parsed the same way as `:k({ ... })`, so the `.AST` of the
second lost its `Circumfix::Parentheses` (rakudo keeps them; only the brace
form holds the `Block` or `HashComposer` directly). The brace form is now its
own colonpair form, and its block or hash composer gets its range, body and
statement list included (#15780).
