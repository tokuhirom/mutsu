# A built-in method called on a user class is resolved once, not per call

`resolve_method_cached` caches a method's resolution under `(class, method)`.
It also stored a miss ("no user method answers this name"), but never read
that entry back. So every call of a built-in method on a user class re-ran
the multi type-key pass and the candidate-sequence resolution, only to reach
the same "nothing" again. `G.parse` on a user grammar is the commonest
example; a call that carries a named argument such as `:actions(...)` paid
more.

The entry could not simply be trusted, because a multi set whose candidates
do not bind *these* arguments may still answer another call. A miss is now
recorded only when the class's candidate sequence for the name is empty,
which no argument list can change, and a recorded miss ends the lookup. The
cache is cleared on every registry generation change, as before, so a method
added later with `.^add_method` is still found (#18282).

On #18207's grammar repro, `resolve_method_cached` now costs 114 instructions
per `G.parse` call (profiling build), and `resolve_via_sequence_cache` runs
once for the whole program instead of once per parse.
