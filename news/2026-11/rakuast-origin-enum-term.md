# RakuAST: origins of an enum's term

The `term` of `enum E (a => 1, b => 2)` and `enum E <p q>` carries the ranges of the text it was
written as: the parenthesised list is parsed again as a fragment and its ranges (`Parentheses`,
`SemiList`, the statement, the comma list, each `FatArrow`) copied onto the converted term, and a
`<...>` word quote spans its brackets with the literal inside them. A single pair and colonpair
values (`:A(1)`) differ in tree shape, not only in ranges, and stay as they were (#15780).
