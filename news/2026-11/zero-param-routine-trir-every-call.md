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

The lost write of a resuming CONTROL handler no longer reproduces on current
`main`. A regression test now pins it.

With that, every parameterless routine with a TRIR chunk runs it from the
call cache, as a routine with parameters does.
