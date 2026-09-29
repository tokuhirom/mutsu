# `.^lookup` on builtin types walks the same chain as `.^mro`

`.^lookup`, `.^find_method` and `.^can` find a builtin type's native methods by
scanning the builtin method rows of each ancestor. The ancestor list came from
the registry's cached MRO. For several bootstrap classes that cache stopped at
the class itself, so `Promise`, `Channel`, `Lock`, `Supplier` and `Thread` could
not see `gist`, `Str` or `defined`. For a registered class whose MRO had not been
computed yet, the ancestor list was a guessed `[T, Cool, Any, Mu]`.

The probe now walks the chain `.^mro` reports: the class's parents, with the
builtin type catalog's chain spliced in for each catalog type, ending in
`Any`/`Mu`. It never guesses `Cool`. That retires the last of ADR-0051's private
ancestry answers for introspection (#10132). `Any.list` also gained its
native-method row, so `Any.^lookup("list")` and `Date.^lookup("list")` are
defined, as in Rakudo.
