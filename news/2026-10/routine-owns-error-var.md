# A routine owns its `$!`; a block shares its scope's

`$!` now follows the rule `$/` got in the previous slice. Every routine has
its own `$!`, initially `Nil`, and a block uses the `$!` of the scope it is
written in (#12902).

Before this, only some call paths shadowed `$!` on entry. A sub with
parameters therefore read its caller's `$!`.

In the other direction, a `try` inside a block that another routine called
wrote `$!` into the block's own frame. That write was dropped on return, so
`t { try { die } }; say $!` printed `Nil`.

The frame root of a routine now owns `$!` for every call path. A block that
touches `$!` shares its scope's `$!` through a cell, and `try` / `CATCH` store
into that cell. This is ADR-12529 phase 3, slice 11.
