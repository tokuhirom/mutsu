Semicolon-dimensional element binds now take the source value rather than
aliasing its container. Both shaped and nested arrays keep that value in a
readonly slot, which can be rebound without writing through its old binding.
Chained bracket subscripts retain their container-sharing semantics.

A one-dimensional shaped element rebind now replaces its old container with
the new source container. Reads and writes follow the new source, including
when the shaped array or source scalar is already shared.

Two-level chained binds now share the existing cell-preserving deep bind
walk instead of copying away the source container. Chained element stores
also honor the readonly state of the slot's bound cell.

Itemized bound element holders now preserve their alias separately from
ordinary value-sharing holders. Array and Hash values keep their element
rendering while the source stays bare, and assignment through a bound Scalar
continues to update its source.
