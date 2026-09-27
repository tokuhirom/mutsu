# Assigning to an adverbed subscript dies like Rakudo

`@a[1]:v = 31` (and the `:k`, `:kv`, `:p`, `:!v` forms, on arrays and hashes)
used to die with the internal `Unknown call: __mutsu_subscript_adverb`: the
parser lowered the assignment as a call to an rw *routine* of that name. Any
internal `__mutsu_*` call on the left of `=` is now evaluated and its value is
the assignment target, and assigning to a non-routine expression value follows
Rakudo: a container is written through, an `Array`/`Hash` is list-assigned
into, an immutable `List` reports its first non-container element, and any
other value dies with `X::Assignment::RO: Cannot modify an immutable Int (10)`.
The same rule replaced the old `cannot assign through non-callable value`
message for `(1 + $x) = 3` and `(1, 2) = 3`. (#9811)
