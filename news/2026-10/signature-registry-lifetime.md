# A Signature value's SigInfo is freed with the value

`SIG_REGISTRY` kept the `SigInfo` of every `Signature` ever built for the life of
the process. `InstanceAttrs` of class `Signature` now count themselves per
instance id (a new value, a `.clone` copy, a reblessed alias) and the last one
to die removes the entry (`sig_instance_born` / `sig_instance_died`).

Release build, 50000 iterations of the issue's loops (B/iter retained, after a
warm start): `C.^lookup('new')` 149-155 (raku 131), `Str.^find_method('chars').signature`
7-8 (raku 0), `:($a, $b, :$c)` 0 (raku 83), `:($a, $b).clone` 1 (raku 4).
Unit tests in `value::signature` pin that a dropped value and a dropped deep copy
free the entry.
