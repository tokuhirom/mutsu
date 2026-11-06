# `$param =:= Int` for a typed readonly parameter

A nominally typed readonly parameter (`Int $x`, `Int :$x`, `Int $x?`) is bound straight to its
argument in rakudo, so `$x =:= Int` is True when the argument is that type object (an untyped,
`is copy` or `is rw` parameter owns a Scalar and stays False). mutsu answered False for the typed
forms; the compiler now records such parameters and compares by value identity against a literal
or type object (#16959). Cooklang's `$split-by !=:= CookBase` gate depends on it.
