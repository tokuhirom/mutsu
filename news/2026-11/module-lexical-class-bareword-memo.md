# A module's `my class` read as a bareword keeps its site memo

A bareword naming a module's unit-level `my class` (Array::Sorted::Util's
`NotFound`, read in `nqp::istype($i, NotFound)` once per `inserts` call) was
resolved through the whole bareword chain on every execution. The per-site memo
(ADR-0121 D3) kept such a type only when `resolve_unit_lexical_type` had
rewritten the answer; when the chain itself answered the ADR-0047 storage name,
the site was never remembered.

The memo now also keeps that answer when it is the unit-level lexical type the
code's own compunit declared under that spelling, which is the answer
everywhere in the unit; a block-scoped class of the same spelling is still not
remembered. A read of such a name went from ~30.9k to ~17.5k instructions
(profiling build, callgrind slope), and a Concurrent::PriorityQueue push with
64 duplicate keys from 1.33M to 1.30M.
