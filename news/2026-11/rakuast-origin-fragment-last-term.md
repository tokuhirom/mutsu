# RakuAST: deterministic origins for a re-parsed fragment

`.origin` on a term could differ from one run of the same program to the
next (#18506). The parser skips noting a term that is the one it noted just
before, recognized by the addresses it was read from. A fragment parsed again
from a fresh `String` (an origin fallback such as the inside of
`("nf" without 3)`) could be given the address of a buffer an earlier parse
had read from and dropped. Its first term then passed for that earlier one
and was never noted, so in some runs (depending on address-space layout) a
literal lost its origin. Each parse that notes terms now starts with no last
term and restores the enclosing parse's on its way out.
