# Multi candidates differing only in a definiteness smiley are ambiguous

`multi f(Int $x)` next to `multi f(Int:U $x)` (or `Int:D`) tie in Rakudo, so a call
both bind raises `X::Multi::Ambiguous`. mutsu silently took the first candidate because
the dispatch shape used for tie detection included the smiley. The shape now ignores
`:D`/`:U`/`:_` (#13064).
