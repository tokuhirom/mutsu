# Push-family native wrap reads the slot, not a seeded env mirror

`CallMethodMut` seeded the env mirror of a scalar-held container from its local slot
before a `push`/`append`/`unshift`/`prepend`, only so `native_int_element_constraint`
could read the container's element-type metadata by name. That helper now reads the
slot of a declared local directly and the seed is gone (#14701). Still open on that
issue: the `:delete` seed, which needs a `StorePlace` in the delete handler.
