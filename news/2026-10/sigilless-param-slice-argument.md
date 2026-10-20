# A slice passed to a sigilless parameter is written through

`sub d(\t) { t = (7, 8) }; d(@arr[0, 2])` used to die with "Cannot modify an
immutable List": the parameter bound a List of the selected values. Rakudo binds
the selected elements' containers, so the assignment lands in `@arr` (`[7 2 8]`).

A slice argument now reaches the container-mode argument producer
(`IndexArgRef`, ADR-0059 Slice 3) like a single subscript does. A callee that
binds its argument's location gets the selected elements' locations as one
bound slice: promoted cells, or deferred tokens for elements that do not exist
yet, so a read never vivifies them and the first write does. The slice is
identified by a flag its producer sets rather than by its contents, so an empty
selection is a writable selection of nothing. A scalar `is rw` parameter still
refuses a slice with `X::Parameter::RW`.

On the way, a method call taking `%h<a c>` toward a container-binding parameter
no longer encodes the whole key list as one `"a c"` key (#13786).
