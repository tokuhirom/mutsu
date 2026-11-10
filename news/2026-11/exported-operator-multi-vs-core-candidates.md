# An operator multi a `sub EXPORT` hook hands out competes with the core candidates

A `multi sub infix:<∈>(Mu $e, Mu $c)` is wider than rakudo's core `∈` candidates, so the core operator keeps
answering ordinary operands. When the multi reached the unit through a `sub EXPORT` hook
(`Map.new(M::EXPORT::DEFAULT.WHO.pairs)`) the captured candidate had no definition to rank against the core set
and always won, so `1 ∈ [1, 2]` answered the multi's value. The candidate's signature now takes part in the
ranking, and `∈` / `∋` have a modelled core candidate set (#17297, Qwiratry `t/operator/precedence.rakutest`).
