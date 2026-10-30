# `when TYPE` reads a Proxy topic by value

`given $value { when SomeRole {...} }` compared the Proxy container itself when `$value` was a
pointy-block parameter bound to an `is rw` result, so the type test failed where `$value ~~ SomeRole`
and rakudo succeed. The `when` clause now FETCHes a Proxy topic, as the `~~` operator does. Red's
`Json` column `AT-KEY` STORE (`.data<copy> = .data<items>`) took the wrong branch and tried to
deflate an AST node; Red t/87 now passes.
