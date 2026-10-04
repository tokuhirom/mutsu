# Element stores through an rw accessor vivify `Any` and refuse scalars

`$o.x[0] = 5` through `has $.x is rw` did nothing when the attribute still
held `Any`: the accessor-lvalue store found no Array or Hash to modify and
returned quietly. It now autovivifies an Array (positional) or Hash
(associative) and installs it through the setter, so `$o.x.raku` is `$[5]`
as in rakudo; a non-rw accessor still refuses that write-back.

The opposite case -- the accessor's location holding a defined non-container
such as `3` (an rw attribute, or an rw method returning `%!h<a>`) -- is now
refused with `X::Assignment::RO` ("Cannot modify an immutable Int (3)") for
an instance receiver too, instead of being silently dropped.
