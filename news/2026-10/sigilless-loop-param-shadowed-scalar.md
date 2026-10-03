# Sigilless loop parameter no longer aliases a same-named scalar

`for (@b,) -> \x { x = Empty }` failed with "Cannot modify an immutable Array"
when the enclosing scope also declared `my $x`, because the read-only check
looked only at the scalar's local slot. It now also probes the by-name binding
of the sigilless parameter (#11361).
