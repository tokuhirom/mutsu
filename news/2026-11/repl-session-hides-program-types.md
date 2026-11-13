# A REPL session no longer sees the running program's types

In rakudo, a `Perl6::Compiler.eval` session started without an outer context gets a fresh `GLOBAL`. The types of the program that runs the session are not in scope there. #17087 got that right for a type a module declared. #17642 now does the same for the main program's own types and for the parse:

- **Recording.** The types the main program's lineage declares (classes, roles, grammars, enums, from the main unit and the `EVAL`s it runs) are recorded as they are registered.
- **Hiding.** A unit of a REPL session no longer resolves those types by bare name. This applies to its code at run time, to `EVAL`'s undeclared-name check, and to the type names its parse is seeded with.
- **Parsing.** The parse is no longer seeded with the type names of a module the session has not merged. So `Out(...)` in a session line calls the session's `sub Out` instead of parsing as a coercion to the sandbox module's `class Out`.

A session that declares a type under a name the program already uses is not supported yet (#17652).
