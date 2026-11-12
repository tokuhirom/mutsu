# Assigning through an rw method's Proxy no longer FETCHes it

`Type.attr = v`, where `attr` is an `is rw` method answering a `Proxy`, ran
the Proxy's STORE and then its FETCH: the assignment builtin's result went
through the rw-routine return FETCH. Rakudo runs only the STORE, so a
write-only Proxy whose FETCH dies (Red's legacy `^migration` blocks) can now
be assigned through.
