# Explicit `.list` `is rw` loops write back to scalar variables

`for $scalar.list -> $value is rw { ... }` now aliases the scalar's own
container when `.list` produces its single item. Mutations therefore remain
visible through the source scalar, including escaping closures and scalar
parameters such as Email::MIME's in-place header decoding. The compiler keeps
the positional-dereference `@$scalar` and multi-element List paths distinct.

Pinned by `t/control/for-scalar-list-rw-writeback.t`.
