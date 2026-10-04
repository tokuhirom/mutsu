# Named defaults in RakuAST regex pointy arguments

`EVAL` now accepts a RakuAST regex whose subrule colonpair contains a pointy
block with one named scalar parameter and a default, such as
`-> :$candidate = 42 { ... }`. The regex source renderer reconstructs that
signature; the existing compiler and VM evaluate the default when the block is
called during matching.

The focused regression checks Rakudo's RakuAST shape, source and constructed
trees, an explicit named override, and an outer lexical changed between
matches. ADR-0088 §63 records the supported boundary.
