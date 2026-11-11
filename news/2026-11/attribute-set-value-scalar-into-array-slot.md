# `Attribute.set_value` binds a Scalar into an `@`/`%` slot

`Attribute.set_value($obj, $array-in-a-scalar)` binds its argument
(`Mu \value`), so an `@` or `%` attribute ends up holding that Scalar:
`get_value` then hands out a container (`nqp::iscont` is 1) and
`nqp::assign` through it stores into the slot. mutsu reported a container
there but bound it read-only, so the assignment died "Cannot assign to a
readonly variable or a value", and a clone lost the Scalar. EEL 0.3's state
copier forks every continuation this way (#17418, #16888). The slot now holds a
Scalar of its own, and a clone gets a fresh one. Unlike rakudo, that Scalar is
not yet shared with the source variable.
