# A key-decided multi dispatch settles its tie analysis once per argument-type key

A call through a captured multi family (a `CArray` read goes through NativeCall's
`postcircumfix:<[ ]>` candidates) ran the tie analysis of `settle_ranked_matches` on every call:
dispatch shapes per matching candidate, the `is default` scan, the ambiguity check, with about 17
allocations. When no step of the stage program carries a value check (`where`, `subset`), every
verdict comes from the argument-type key, so that analysis gives the same answer on every call of
the key. The program now remembers its settled winner (`StageProgram::settled`) and returns it
directly; an ambiguity, an exception or any program with a value check still settles per call.

Callgrind slope (profiling build, second run, 500 -> 1500 reads of `$x = $c[$_ % 500]` on a
`CArray[num64]`): 112.0k -> 104.4k Ir per read (-7%). Refs #16373.
