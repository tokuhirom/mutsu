# Subscript assignment to an immutable Map names the key

`$map<a> = 5` on an immutable `Map` (also a `%`-variable bound to one) raised
`X::Assignment::RO` ("Cannot modify an immutable Map (Map)"). Rakudo dies with
`Cannot change key 'a' in an immutable Map` for an existing key and
`Cannot add key 'b' to an immutable Map` for a new one, the same as `.ASSIGN-KEY` (#14134).
The wording is built once by `refuse_map_assignment`, shared by the method row and the
subscript store.

Closes #14294. Pinned in `t/collections/transform/map-assign-key-immutable.t`.
