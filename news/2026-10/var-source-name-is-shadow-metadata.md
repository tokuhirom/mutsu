# A parameter's source-variable record follows the parameter

When an `@`, `%` or raw parameter is bound from a caller variable, the binder
records that variable's name so `.VAR.name` can report it (Text::CSV's
`@kh.VAR.name ne "element"`). The record was an env entry that every closure
created in the routine copied, whether or not the closure used the
parameter, and that a return merge could copy back into the caller.

It is now treated like a typed lexical's constraint record: metadata of its
parameter, captured only by a closure that uses the parameter and kept out of
the caller on return. On the FunctionalParsers EBNF parse, by-name chain walks
fell 21%. This is part of ADR-12529 phase 1.
