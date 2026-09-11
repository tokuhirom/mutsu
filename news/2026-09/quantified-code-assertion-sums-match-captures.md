# Quantified code assertions can sum Match captures

A code assertion after a quantified capture now numifies its Match values when
using `.sum`. This lets the assertion retry with the captures from a shorter
quantifier candidate and accept the candidate selected by the predicate.

Pinned by `t/regex/match/quantified-code-assertion-capture-backtracking.t`.

Closes #7904.
