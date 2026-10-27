# (<=), (<), (>), ⊆, ⊂, ⊃ honour an imported overload

The set comparison opcodes never asked for a user `infix:<(<)>` (or `(<=)`, `(>)`, `⊆`, `⊂`, `⊇`, `⊃`):
they coerced both operands to sets directly, so an overload a module exported (Red's `(<)` over a
column and a positional, SQL `IN`) was bypassed and `grep(*.name (<) <b c>)` compiled to `WHERE 0`.
The opcodes now offer the operands as written to the user candidates first, like the six binary set
operators. Red t/19-in-sql goes further.
