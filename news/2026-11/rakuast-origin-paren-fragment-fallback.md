# RakuAST origins of parenthesized expressions with repeated terms

When the conversion copies a term, it places the copy by the term's shape.
It finds the range of the one unplaced term of that shape in the statement.
When the statement holds several such terms, as in
`((1, 2) Z (3, 4) Z (5, 6)).List, 1` where `1` appears twice, it cannot
tell which range the copy takes, so it placed none. For a lone expression
statement in parentheses, the text inside the parentheses is now parsed
again as a fragment, and its ranges are copied onto the nodes left
unplaced, as rakudo 2026.11 places them.

Measured over all 922 `t/rakuast` files, 845 now give exactly rakudo's
origins, and this fallback places nothing new where rakudo places nothing
(#15780).
