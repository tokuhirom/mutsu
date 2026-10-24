# A `Bool` method's error unwinds the statement; user control exceptions resume

Two halves of the same Red gap (`Model.^all.grep({ .a == 1 && .b == 1 })` counted 2
rows instead of 1):

- The truth test of an instance (`&&`, `||`, `?:`, `if`, `!`, `?`, `so`) swallowed
  whatever its own `Bool` method raised and went on with the raw truthiness, so a
  `die` in `Bool` printed its CATCH output and then kept running the statement.
  The error now unwinds the statement, as in rakudo. A type object's `Bool` keeps the
  old lenient answer (a role's abstract method reached through the role itself).
- A control exception of a user-declared class (`class CX::Mine is X::Control`)
  thrown with `.throw` is offered to the active `CONTROL` handlers at the throw
  site, so a handler's `.resume` continues right after the `.throw`, also from
  inside a callee. Red's `Red::AST.Bool` throws `CX::Red::Bool` and its
  `what-does-it-do` resumes it to discover the branches of a block.
