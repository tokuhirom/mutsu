# Assignments to an operator, pair or adverb result are the assignment node

`("a" R~ "b") = 1`, `(key => $v) = 1`, `@a[0;0]:!exists = 1` and `try @a[0;0]:foo = 1` were
parsed straight into the `__mutsu_assign_callable_lvalue` call, which `.AST` refuses, so
four whitelisted roast files (`S03-metaops/reverse.t`, `S06-other/pairs-as-lvalues.t` and
both `S32-*/multislice-6e.t`) failed only under `MUTSU_RAKUAST=1`. The reverse metaoperator,
a parenthesised pair and an `:exists` result are now targets `rakuast::assign_lower` writes
through, a spelled subscript with adverbs is read through its spelling, and the statement
parser builds the node for a call target and for the `try` it spells as written. #15012.
