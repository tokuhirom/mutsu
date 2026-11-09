# `∩` / `(&)` bind tighter than the other set operators, and a `multi` keeps the level

Rakudo puts the set operators `(&)` and `∩` at the junctive-and level and
`(|)`, `(+)`, `(-)`, `(^)`, `∪`, ... at the junctive-or level; mutsu parsed them
all at one level, so `(4,5) ∪ (2,3) ∩ (3,4)` grouped left to right. They now
group as in rakudo. A `multi sub infix:<∩>` that redeclares a core set operator
extends it and keeps its precedence (it used to take the additive level, so
`2 ∩ 3 + 1` read as `(2 ∩ 3) + 1`) -- Qwiratry's `t/operator/precedence.rakutest`
(#13370).
