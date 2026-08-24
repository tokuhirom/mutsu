# Direct smartmatch results no longer execute a `for` body

`for EXPR ~~ /regex/ { ... }` now follows Raku's list-context behavior. A
successful `Match` produced directly by the smartmatch is an empty list, so the
loop body is not executed. An itemized scalar containing the same `Match` still
iterates once.

