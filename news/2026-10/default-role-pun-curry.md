# A default role pun no longer composes the explicit `E[Any]` curry

A parametric role whose parameters all have defaults puns to its default curry. mutsu
recorded that curry as the spelled `E[Any]`, so `E.new.^roles` showed `(E[Any])` and
`E.new ~~ E[Any]` was `True`. The pun now records the bare role, matching Rakudo:
`.^roles` is `(E)` and `E.new ~~ E[Any]` is `False`, while an explicit `E[Any].new` is
unchanged (#11535).
