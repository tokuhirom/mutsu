# `say $parts[0]` on an `IO::Path::Parts` no longer swallows the pair

`IO::Path::Parts` does `Positional`: `$parts[0]`/`[1]`/`[2]` answer the
volume/dirname/basename as an ordered `Pair` (`volume => C:`). Handed
straight to `say`/`put`/`print`/`note` — not through a variable, not through
a method call — the pair vanished: `say $parts[0]` printed an empty line
while `say $parts[0].gist` printed correctly.

The cause was a stale `Value::pair` (the NAMED-argument marker flavour;
[ADR-0021](../../docs/adr/0021-argument-namedness-is-a-call-site-property.md))
in the two places that build an `IO::Path::Parts` positional pair on demand:
the `AT-POS` native dispatch and the `DeitemizeZen` opcode's per-element
rebuild. ADR-0021's P3 flipped every other data-minting site for this type
(`.flat`/`.Slip`/`.cache`/`.eager`) to the positional flavour
(`Value::value_pair`) already, but these two were missed. `say`'s in-band
named-marker filter — which exists precisely so a genuine `key => value`
written at the call site is excluded from the printed output — mistook the
freshly minted pair for that marker and dropped it.

Both sites now mint the positional flavour, matching every other
`IO::Path::Parts` pair producer.
