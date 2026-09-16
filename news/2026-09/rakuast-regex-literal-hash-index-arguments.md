# Regex subrule arguments retain literal hash-index provenance

RakuAST conversion now preserves `%hash<key>` in an argumented regex subrule as
`RakuAST::Postcircumfix::LiteralHashIndex`, rather than reporting the brace-form
`HashIndex` node. Lowered regex trees retain the angle spelling and continue to
resolve the current hash value only during matching.
