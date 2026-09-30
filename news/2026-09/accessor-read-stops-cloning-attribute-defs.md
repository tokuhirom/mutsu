# A plain accessor read stops cloning every attribute definition

`bench-class` stepped from 575k to 665k allocations (+15.7%) when the
generated accessor learned to decontainerize its result (#9807, #10229). The
fast accessor read (`try_fast_accessor_read`) had started calling
`collect_class_attributes` on *every* read to learn whether the attribute was
`is rw` — a fresh `Vec` plus a clone of every `ClassAttributeDef` in the MRO,
about four allocations per `$.name`. The declaration only matters when the
value is an Array or Hash (the only values decontainerizing or type tagging
touches), so the lookup now runs only then.

Two neighbouring lookups on the same path built owned `(String, String)` keys
just to miss in maps that are almost always empty: the `is DEPRECATED`
accessor table (two allocations per accessor read) and the method wrap-chain
table (two per method call). Both now return early on an empty map.

`bench-class` (callgrind, profiling build, warm cache): 649,081 → 491,xxx
allocations.
