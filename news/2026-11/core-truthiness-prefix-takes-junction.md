# `!`, `?`, `so` and `not` take a junction whole, even with user candidates

A program that adds its own `prefix:<!>` candidates (Red does, over its AST
types) made `!any()` thread over the junction and answer `any()`. The core
truthiness prefixes take `Mu`, so a junction is their argument, not threaded
over: `!any()` is `True` again, and Red's migration manager no longer drops
every configured model through `!(.^table eq any(@external))`.
