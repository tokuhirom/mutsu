# Later pointy parameters accept hash sub-signatures

A multi-parameter pointy block such as
`-> Str $name, % (:$count = 0, :$limit, |) { ... }` now parses when the
hash parameter is not the first parameter. This is the shape used by
`Red::Driver::Mock` while iterating over `%!when-str.kv`; mutsu previously left
the parenthesised sub-signature unconsumed and reported a misleading `Missing
block` error from the following body.

The later-parameter parser now reuses the ordinary signature parser for sigiled
parameters that carry a sub-signature, keeping named defaults, nested
destructuring, and capture slurpies on the same binding path.

Pinned by `t/routines/signature/for-multi-param-hash-subsignature.t`.
