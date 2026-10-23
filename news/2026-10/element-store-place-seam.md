# Element stores resolve a place, and whole-container stores read the slot's constraint

The named element stores (`@a[$i] = $v`, `%h<k> = $v`, and their nested and
deep-nested forms) now resolve their target once, at the start of the store,
to a `StorePlace` (ADR-14632): the declaring slot for a plain lexical the
running code declares, the name for everything else. Every container access
of the three store bodies goes through that place's methods
(`place_env_get`, `place_get`, `place_root_mut_tracked`, `place_set`, ...)
instead of a by-name env call of its own. A `Slot` place still resolves by
name, so nothing changes yet; the next step flips those methods to the slot.

`MUTSU_STORE_PLACE_CHECK=1` compares, at every store with a `Slot` place, the
container in the slot with the one the by-name resolution reaches, and prints
each disagreement on stderr. It is the inventory of writers that update one
copy of a binding and not the other, which have to be fixed before the flip.

The whole-container stores (`@a = ...`, `%h = ...` through `SetLocal` and
`AssignExprLocal`) pass the slot they hold to `coerce_typed_container_assignment`,
`coerce_hash_var_value` and the container writethrough helpers, so they read
the constraint from the declaring slot's record as the element stores have
since #14645. A by-name store (`SetGlobal`, `MY::`) still asks by name.
