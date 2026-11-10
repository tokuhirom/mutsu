# Set relations parse at the chaining level

The iffy set operators (`∈ ∋ ⊆ ⊇ ⊂ ⊃ (elem) (cont) (<=) ... !(elem) ∉ ∌ ≡`) sat at the junctive
level, so `1..2 ∈ [1..2]` parsed as `1..(2 ∈ [1..2])`. They now parse at the chaining level, looser
than `..`, as in rakudo (found by Qwiratry's `precedence.rakutest`). Chaining a relation with
another comparison (`$a ∈ @b == True`) is still left-associative, not a rakudo chain.
