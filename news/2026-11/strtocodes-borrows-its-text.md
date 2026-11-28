# `nqp::strtocodes` decomposes a Str without copying it first

JSON::Fast's string scanner hands every string with an escape or a non-word character to
`unjsonify-string`, as `nqp::strtocodes(nqp::substr(...), NORMALIZE_NFD, nqp::create(NFD))`.
The op stringified its first operand into a fresh `String` before decomposing it: an allocation
and a copy, about a third of the op for the short strings a META6 document holds. A plain Str
operand is now decomposed where it lies; any other operand is still stringified first.

`bench-json-fast-decode` (`scripts/bench-det.sh`, `+jit`, local A/B): 1,335.2M -> 1,313.7M
instructions (-1.6%), 622,801 -> 587,532 allocations (-5.7%).
