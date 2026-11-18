# A program's `BEGIN` runs when the parser reaches it

A statement-form `BEGIN` at the top level of a program now runs while the
program is being parsed, at its place among the `use` loads, as rakudo runs it:

```raku
BEGIN %*ENV<MY_MODULE_DEBUG> = 1;   # runs here, at parse time
use MyModule;                        # loads after it, still at parse time
BEGIN say "b";                       # prints before a later syntax error
```

Until now every `BEGIN` waited for the BEGIN-time prologue, after the whole
program was parsed, and so did every `use` written after one: such a module
loaded in the prologue, and the rest of the parse knew only what a static scan
of its source could tell. A `use` after a `BEGIN` now loads at parse time too,
so its computed export names, operator precedence and declarators reach the
parse, and a `BEGIN`'s output comes before a syntax error found after it.

This applies to a `BEGIN` whose body reaches nothing of the program, which has
no frame yet while it is parsed: no lexical of the program, only core routines,
types and terms the program has not redeclared or imported, no declarations
and no dynamic lookup, and no BEGIN-time effect left to the prologue before it
(a computed `constant`, a user trait, a `use` that did not load at parse time).
Any other `BEGIN` keeps its prologue position, and so does everything after it.
Part of ADR-17558 (slice 5a).
