# `my Int &foo` parametrizes the Callable, not the value

A type on a `&`-sigil variable now constrains the routine it holds as
`Callable[Int]` (its return type, `&foo.of`) instead of being checked against
the routine itself, so `my Int:D &foo = sub (--> Int:D) {}` assigns and
`&foo.of` answers `Int:D`, as in rakudo. A routine with another return type
and a plain value are still rejected (#16439).
