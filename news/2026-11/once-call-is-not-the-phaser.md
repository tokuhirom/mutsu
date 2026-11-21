# `once(...)` calls a routine named `once`

`sub once() { 42 }; say once()` printed `()` instead of `42`: the term parser took any `once`
keyword for the phaser, so `once()` became the phaser over an empty list and never called the
sub -- and `with once() { ... }` never evaluated its condition (#18228). An identifier directly
followed by `(` is always a call in Raku (rakudo reports an undeclared routine when there is no
`&once`), so `once(` is now a call; `once { ... }` and `once STATEMENT` stay the phaser.
