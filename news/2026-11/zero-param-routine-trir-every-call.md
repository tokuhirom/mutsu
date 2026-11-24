# A parameterless routine runs its TRIR chunk on every cached call

#18080 let a parameterless routine take its TRIR chunk from the call cache.
It limited that to self-contained, program-scope bodies, because two wrong
answers showed up under `EVAL(slurp($file).AST)` (#19136).

The free-variable one had a single cause. TRIR resolves a routine's free
variables by the routine's name. It tried a routine-nested sub's
per-activation binding before the program-scope routine's own mainline
bucket. A program-scope `sub sv { $s }` beside a routine's nested
`sub sv { $s }` of the same name, package and file therefore read the nested
sub's `$s` once that routine had run. The bytecode path already declines the
nested sub's cells for a program-scope routine (#13900). TRIR now does the
same: a chunk that a mainline bucket runs resolves in that bucket and never
takes a routine-nested sub's activation.

The lost write of a resuming CONTROL handler was a general bug, not a TRIR
one. A handler raised under a pushed frame (a call) writes its change straight
into the installing block's live slot, which leaves that block's env entry
behind. The next `warn` raised in the block itself, with no frame pushed, then
seeded the handler from the env by name, so the handler started from the stale
value and one increment was lost (`after 6: 5`, where rakudo says 6). When the
installing activation is the executing frame, the handler now seeds from that
frame's slots, which are the variables there too.

With that, every program-scope parameterless routine with a TRIR chunk runs
it from the call cache, free variables and all. A block- or routine-declared
one still keeps the bytecode path: it resolves its names through an
activation the cached call does not hand TRIR.
