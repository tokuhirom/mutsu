# `(a X b) X c` keeps the inner product nested

`a X b X c` is one n-ary product with flat tuples, but the compiler flattened the
chain after peeling the parentheses of its left operand, so `(a X b) X c` also
produced flat triples. Only the operand as written is chained now: a
parenthesized product stays one operand and yields `((1, 0), "a")`, as rakudo
does (Red `t/91-cross-product`, "Parentheses retain nested tuples").
