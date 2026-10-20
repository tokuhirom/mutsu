# `.^methods` no longer repeats a method a built-in class shares with a composed role

`Int.^methods` listed 144 entries for 86 names (the `Real` role's rows on top of
`Int`'s own) and `Map` listed `hyper`/`race`/`lazy` twice (`Iterable`'s rows plus the
catalog's). The composed-role walk now goes through the same by-name dedupe as the
class walk, so the most-derived entry wins, as it does for `.^find_method`.
