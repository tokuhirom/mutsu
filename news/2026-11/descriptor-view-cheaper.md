# A container's `$!descriptor` view is cheaper to build

`nqp::getattr(@a, Array, '$!descriptor')` hands out a `ContainerDescriptor`
that views the container's own element type and default (ADR-0121 D3).
JSON::Fast reads one for every array and hash it builds. Each read built
the view through a `String`-keyed map, re-interning every attribute name. It
also registered the view in the DESTROY queue's process-wide refcount table,
although `ContainerDescriptor` has no `DESTROY`.

The names are now interned once and the view skips that bookkeeping. A
`bindattr` of a descriptor no longer builds a view just to learn that its
receiver is an Array or Hash.

The 100-record SPDX decode (JSON::Fast 0.21.0) went from 54.33M to 53.50M
instructions (−1.5%, callgrind).
