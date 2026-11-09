# `class C {...}` is a stub package in RakuAST

A stub package (`class C {...}`, `role R {...}`) is `is-stub => True` over an empty body in
rakudo; mutsu kept the `...` as the body's one statement, so `.DEPARSE` printed it on its own
lines and `.is-stub` was always false. The tree now matches rakudo's, and `EVAL($ast)` puts the
stub statement back when lowering.
