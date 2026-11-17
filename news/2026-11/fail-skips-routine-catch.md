# `fail` passes a CATCH on its way out of the routine

`fail` returns a Failure from its lexical routine. It is a return, not an
exception, so in rakudo a `CATCH` it passes on the way does not handle it. mutsu
threw it as an exception, so a `CATCH` in the same routine, or in a routine
between a closure's `fail` and the closure's own routine, caught it.
`sub f { CATCH { default { ... } }; fail "x" }` returned `Nil` after running the
handler, where rakudo returns the Failure. Now only `try` (which is fatal),
`use fatal` and a `fail` outside any routine throw.

Red's `automatic-transaction` wraps the user's `red-do` block in a CATCH,
KEEP and UNDO. A `fail` in that block was rethrown as an error instead of being
returned from the caller as a Failure.
