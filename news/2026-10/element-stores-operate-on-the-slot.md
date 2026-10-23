# Element stores into a declared local operate on its slot

A named element store (`@a[0, 1] = 7, 8`, `%h<a b> = 1, 2`, `@a[1][0] = 9`
and the deeper chains) into a local the running code declares now takes its
container from the variable's slot and mutates it there (ADR-14632). Before,
the store looked the variable up by name in the env, worked on the env's
copy and copied the result back into the slot. The slot was already what
every read used.

A write that replaces the container also points the env mirror at the new
one, so the by-name readers that remain (an `EVAL`, `MY::`) see the store.
Before-store seeding and after-store write-back are no-ops for these stores
now. A target the code does not declare, an aliased one, or one a
cross-thread lane may answer still stores by name.

The one divergence the ADR's check found, a lazy `@` array, takes the
reified prefix into the slot for the duration of the store and gets the
rebuilt lazy list back in both places.
