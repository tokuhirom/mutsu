# An anonymous `(...)` parameter no longer binds a named argument

`sub p(($x))` and `sub p(Pair (:key($k), :value($v)))` accepted `p(a => 1)` by
destructuring the named argument as if it were their positional. The binder now
skips only named-flavour Pairs (ADR-0021) when choosing the destructure target,
so the call dies with "Too few positionals", as in Rakudo. Positional Pairs
(hash iteration, parenthesised pairs) still destructure. Closes #11891.
