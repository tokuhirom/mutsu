# RakuAST: origins of a parenthesized semilist

The statements of a parenthesized semilist such as `(1, 2; 3)` were left
without `.origin` when their terms shared a shape with other terms of the
statement (`say (1, 2; 3), 1`), which the conversion could not tell apart
(#15780). Such a list is now placed from a fragment parse of the text inside
the parentheses, as a lone statement already was; the copy only fills in
nodes that have no range.
