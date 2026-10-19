RakuAST now round-trips `is export` on token, rule, and regex declarations.
Bare and tagged exports are represented by the measured `Trait::Is` node, and
hand-built declaration nodes can carry the same trait list through EVAL.
