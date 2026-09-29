# A mixed-in role's method reaches the enclosing role's multi subs

A method of a nested `my role`, mixed into a value with `does`, runs under the
synthetic package `Any+{R::W}`. The bare-name package walk started from that
mangled name and never reached the role's enclosing package `R`, so a `multi
sub` declared in `R`'s body died with `Unknown function`. The walk now also
climbs outward from the routine's lexical package. This unblocks
`XML::Class::to-xml` (found through the Map::Mapnik ecosystem distribution).
