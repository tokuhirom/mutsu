# SetHash.set and .unset take exactly one positional

`SetHash.set` and `.unset` silently accepted any number of arguments, so
`$s.unset(1, 2)` removed both keys and `$s.set()` was a no-op. They now raise
Rakudo's `Too many/few positionals passed; expected 2 arguments but got N`
error, like `BagHash.add`/`remove`. A single list argument is still iterated
one level.
