# An element bind no longer leaves a name-keyed bound-index table behind

Binding an element (`@a[1] := $x`, `%h<k> := $x`) recorded the index in a
`__mutsu_bound_index::<name>` env side table. The element's own `ContainerRef`
cell already is the binding, and every store writes through it. So the table
was redundant, and it went stale whenever splice moved the element. The only
behaviour it still drove was a wrong one: binding the same element of a shaped
array a second time (`my @s[3]; @s[1] := $q; @s[1] := $q`) died with "Cannot
modify an immutable value", while Rakudo allows it.

The table and its writers are gone (the indexed store, the multidim bind, and
the delete and splice clean-ups). So are its readers: the shaped re-bind
refusal, and the three element fast paths that declined whenever a variable had
any bound element. `MetaNs::BoundIndex` is retired as well, one more slice of
ADR-0097's retirement of name-derived binding metadata (#8087). The helper that
encodes an index for the remaining per-element bookkeeping is now
`encode_element_index`.
