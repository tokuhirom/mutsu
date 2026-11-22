# `Junction.ACCEPTS` answers a Bool

`(1|2).ACCEPTS(2)` is `True` in Rakudo. A junction declares its own `ACCEPTS`,
which combines each value's `ACCEPTS` of the topic by the junction's kind into
one `Bool`, and matches a junction topic as a whole. mutsu autothreaded the
call over the junction's values instead, so it answered `any(False, True)`, and
a junction of junctions for a junction topic.

`ACCEPTS` is now a `Junction` row (ADR-11276) that answers
`topic ~~ junction`, so the method and the smartmatch operator cannot disagree.
