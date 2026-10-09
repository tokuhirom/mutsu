# RakuAST keeps grouping and compound statement provenance

The RakuAST frontend now restores parser parentheses around postfix operands after rendering the Rakudo node shape, which omits one visible layer. This preserves grouped method receivers such as `(1..6).grep(...)` and grouped subscript keys such as `Bag{()}`. The source grouping depth is kept in hidden metadata and bounded during lowering.

A compound assignment that the parser wrapped as a statement now lowers back to that statement form. In a `for` loop, this retains writeback behavior and prevents an earlier grep loop from changing how a later lazy grep pulls its source. The existing quantization and lazy-sequence tests now pass in RakuAST mode, and a focused test checks the read/EVAL round trip against Rakudo.

This is an S10 slice of #7564.
