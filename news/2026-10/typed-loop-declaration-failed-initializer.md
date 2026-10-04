# Typed loop declarations retain their type after a failed initializer

A typed scalar declared inside a loop now holds its declared type object when
its initializer throws. Repeated loop iterations previously reset the binding
to `Any`, which prevented the following type registration from seeding it.
Native typed scalars likewise reset to their native default.
