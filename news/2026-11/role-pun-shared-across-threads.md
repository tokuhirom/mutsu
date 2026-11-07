# A parametric-role pun made on one thread is composed on demand by another

Every thread runs on a copy-on-write snapshot of the declaration registry, so
the pun class of a parametric role (`Conc::Maybe[Any]`) created on one thread
did not exist on the others. A type cached in a shared `Map` (as the Monad
distribution does with its `pun but Some` types) then failed on the second
thread with `Could not find symbol '&Maybe[Any]' in 'GLOBAL::Conc'`.
The creating thread now publishes the pun's recipe (the role and its
type-object arguments) next to the mixin recipes of ADR-11456, and a thread
that misses the name composes it from the recipe. Monad's
`t/14_concurrent_types.t` passes all 21 assertions (it is heavy on a debug
build). Closes #17042.
