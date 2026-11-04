# A hyper operator with a `*` operand is a WhateverCode

`(14, 12, 12) »%» *` and `* «+« (1, 2)` are now WhateverCodes, as in rakudo:
hyper operators are not Whatever-aware, so a bare `*` operand curries the whole
expression like it does for `* + 1`. Priming detection, placeholder counting and
placeholder replacement all treat `Expr::HyperOp` like `Expr::Binary`. As an
index, `(1..10)[(4, 2, 2, 2) »%» *]` now gets the element count and answers
`(5, 3, 3, 3)`; `6.c/MISC/bug-coverage-stress.t` test 14 ("no SEGV in curries +
with") passes (#16402).
