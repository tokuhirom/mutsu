# A user infix operator call no longer re-ranks the core candidate set

When a program declares `multi infix:<*>(UInt $a, UInt $b)`, every `*` has to
decide whether the user candidate out-ranks the operator's core candidates
(ADR-0071). That ranking walked every modelled core signature, `(Int:D, Int:D)`,
`(Rational:D, Int:D)`, `(Complex:D, Real)` and so on, through the string-keyed
`type_hierarchy_distance`. That came to about 30 MRO walks and ~13k instructions
per operator call. It was the largest single cost of the FiniteField-style
operator section of `bench-multi-dispatch` (#10111).

The answer depends only on the operand types, so it is now memoized per
`(operator, candidate, operand type keys)`. The entry is tagged with the
functions-map generation, so it goes stale when a candidate is added or wrapped,
and the key also carries the proto generation and the subset count. A candidate
whose rank reads an argument's value (a type capture) is never memoized. Operands
that do not reduce to a type key (a Junction, a mixin, a container) are not
memoized either.

On `multi infix:<+>`/`infix:<*>` with `callsame() mod $*modulus` (the
benchmark's operator section), one loop iteration with two operator calls went
from 81.4k to 66.5k instructions under callgrind, **-18%**.
