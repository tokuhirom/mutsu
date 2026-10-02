# An INIT/CHECK in a class declared inside a routine runs at program start

`sub never-called { my class K { INIT say "init" } }` printed nothing, and a routine that did run
the declaration ran the phaser at that call. Rakudo runs every `INIT` once before the mainline
(source order) and every `CHECK` once at the end of compilation (reverse order), wherever it is
written.

The body of a class declared inside code is now walked as a scope, like a routine's: its `my`
lexicals get static cells and a routine its body declares is copied into the lifted phaser. Every
`INIT`/`CHECK` in the body or in a method is lifted to the unit's own sequence, so it runs at the
right time, sees a body lexical in its static state, and can call the class's own `sub`.

A phaser that names the class itself (which does not exist at the unit's level), one in a grammar
body, and a `BEGIN` in such a class keep their old handling.

Fixed on the way: a `sub` declared in a method of a package class was not seen by an `INIT` of
that method (`Unknown function`).
