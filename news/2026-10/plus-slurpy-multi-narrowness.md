# `multi f($x, +@a)` no longer outranks `multi f($x)` for `f(1)`

A single-argument slurpy parameter (`+@a`) was ranked as a typed positional
(its implied `Positional`) instead of a slurpy, which made the nominal tier
prefer `multi f($x, +@a)` to `multi f($x)` for `f(1)`; rakudo ignores slurpies
in narrowness and lets the fixed-arity candidate win. The effect reached any
`nextwith`/`callwith` chain whose first candidate is the fixed-arity one
(`multi method refresh(\model) { nextwith model, |@columns }` in Red never ran,
leaving `t/56-refreshable` stale). `+@a` now counts as a slurpy in the typed
count and the writable-trait count, like `*@a`.
