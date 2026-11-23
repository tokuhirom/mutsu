# `^add_multi_method` on a composed class waits for the next `^compose`

rakudo keeps a candidate that `.^add_multi_method` adds to an already composed
class in `multi_methods_to_incorporate`. Dispatch does not see it until the
class's next `.^compose`, so `$obj.g("a")` is still `X::Multi::NoMatch` in
between. mutsu installed the candidate at once.

`mop_add_multi_method` now holds a candidate for a composed class in the
registry's new `deferred_multi_methods`, and `mop_compose` installs the class's
held candidates in the order they were added. A class that has not been
composed yet still takes the candidate directly, as before (`^can` already
hides it until compose).

The other half of #18868 remains open: an added `my method`'s invocant is `Mu`
in rakudo, so `(Mu: Str)` ties with `(Late2: Cool)` and the call is ambiguous.
mutsu does not record that invocant type at all (`my method g2(Str $x) {}`
reports the signature `($: Str $x)`, where rakudo reports `(Mu $: Str $x, *%_)`).
