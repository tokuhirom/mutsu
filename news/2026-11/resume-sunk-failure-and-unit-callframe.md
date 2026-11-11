# `.resume` past a sunk Failure, and the mainline's `callframe` code

A `Failure` that explodes when it is sunk now throws resumably, as Rakudo's
`Failure.sink` does with `.throw`: a `CATCH` that calls `.resume` continues
with the statement after the one that sank it. Before, the sink ops
(`SinkPop`, `SinkPopCall`, and the `ThrowIfFailure` that ends a protected
block) were not resume points, so the handler ran from the enclosing region,
the routine returned `Nil`, and an outer call could even see its invocant
replaced by a value left on the operand stack (`No such method 'history' for
invocant of type 'IO::Path'`).

`callframe(1).code`, seen from a routine the mainline calls, is now the unit's
`Block` named `<unit>`, with `.file` the program's file and `.line` 1, instead
of a nameless `Sub` with no file.

Both came from the REPL distribution: its `rakudo-history` method `spurt`s a
history file into `~/.raku`, which does not exist in the ecosystem sandbox,
notes the failure and resumes; and its `repl` sub builds a header from
`callframe(1).code.file` and `.line`. All three of REPL's test files now match
rakudo.
