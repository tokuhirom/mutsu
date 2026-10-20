Semicolon-dimensional element binds now take the source value rather than
aliasing its container. Both shaped and nested arrays keep that value in a
readonly slot, which can be rebound without writing through its old binding.
Chained bracket subscripts retain their container-sharing semantics.

A one-dimensional shaped element rebind now replaces its old container with
the new source container. Reads and writes follow the new source, including
when the shaped array or source scalar is already shared.
