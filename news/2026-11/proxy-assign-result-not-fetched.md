# Assigning through an rw method's Proxy no longer FETCHes it

`Type.attr = v`, where `attr` is an `is rw` method answering a `Proxy`, ran
the Proxy's STORE and then its FETCH: the assignment builtin's result went
through the rw-routine return FETCH. A block ending in such an assignment and
called from a `for` loop (`.(Type) for @blocks`) FETCHed the result again when
the loop sank it. Rakudo runs only the STORE in both cases, so a write-only
Proxy whose FETCH dies (Red's legacy `^migration` blocks) now works.
