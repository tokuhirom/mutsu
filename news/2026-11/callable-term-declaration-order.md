# Callable terms obey source order

`sub term:<foo>` defines a callable term at its declaration. The undeclared-routine scan also treated its bare spelling `foo` as a hoisted routine name. A `foo` written before that declaration was therefore accepted and evaluated as the string `"foo"`, although Rakudo rejects it during compilation.

The scan now keeps the routine's full name available for normal routine lookup and records the bare spelling as a term at the declaration point. An earlier bareword raises `Undeclared routine`; a use after the declaration still calls the term.
