# A user-declared junction operator is list-associative

`sub infix:<&>(*@a, :$adverb)` called through `1 & 2 & 3 :adverb` used to run twice, nested to the left, with the adverb on the
outer call only. A trait-less user `infix:<&>`, `infix:<|>` or `infix:<^>` now defaults to `list` associativity, as the core
junction operators have, so the chain is one call over every operand (`[1, 2, 3]`) with the adverb. A parenthesized operand
stays one operand (`t/types/junction-infix-flat-call.t`).
