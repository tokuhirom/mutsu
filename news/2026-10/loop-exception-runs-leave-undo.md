# An exception leaving a loop iteration runs its UNDO and LEAVE phasers

A `die` (or any ordinary exception) that unwound out of a loop body skipped that
iteration's UNDO and LEAVE phasers, so
`try { for 1..2 { LEAVE print "l"; UNDO print "u"; die "x" } }` printed nothing
where rakudo prints `u l` (#10580).

`OpCode::LoopExitGuard` already ran the iteration's exit queue for
`next`/`last`/`redo`/`return`; it now runs it for exceptions too. The blocker
the issue named was resumption: a CATCH that `.resume`s must not find LEAVE
already run for an iteration that is going to continue. Since ADR-0072 a
`die`/`.throw` offers the exception to every resume-capable CATCH at its throw
site, so an exception that reaches the guard either carries the handlers'
verdict (they ran and did not resume) or was never offered to them (an error
raised by some other op). The guard offers the latter to the handlers itself
before unwinding, and resumes the iteration at the recorded resume point when
one resumes it. The queue therefore runs after the handler, as in rakudo
(`c l1`, not `l1 c`), and never for a resumed exception. `exit` still skips it.
