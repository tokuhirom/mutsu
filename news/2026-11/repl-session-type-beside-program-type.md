# A REPL session can declare a type named like one of the program's

A `Perl6::Compiler.eval` / `REPL.repl-eval` session started without an outer
context does not see the running program's types (#17642). It could not declare
one of the same name either: `class ReplHost { ... }` in a session whose host
program declared `class ReplHost` died with `Redeclaration of symbol
'ReplHost'`, because mutsu's type registry is shared by the whole process.

rakudo gives such a session a fresh `GLOBAL`, so the session's `ReplHost` is a
different type living alongside the program's. mutsu now declares a top-level
`class` / `role` of a session unit whose name is one of those hidden program
types the way it declares a `my class`: under a storage name of its own
(ADR-0047). The session's later units see it as they see an earlier unit's
lexicals, a name hidden from the session resolves to a lexical type of that
spelling in scope before it is reported undeclared, and the program's own
`ReplHost` is untouched. Fixes #17652.
