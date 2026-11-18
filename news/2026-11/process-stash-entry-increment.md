# `PROCESS::<$x>++` creates and increments the dynamic variable

`PROCESS::<$x> += 1` already compiled to a read of the stash entry and a
store through `IndexAssignPseudoStashNamed`, so it created `$*x` when it did
not exist. The `++`/`--` forms went down the nested-element path instead,
which binds the root of the subscript chain to a temporary first: for a
`PROCESS::`, `MY::` or `CALLER::LEXICAL::` stash that temporary is a
snapshot, so the increment landed in it and `$*x` stayed undefined
("Dynamic variable $*x not found").

A pseudo-stash entry with a literal key now skips the bound-root rewrite and
takes the read-increment-write path, whose write-back goes through the
stash's own store, in all four prefix/postfix increment and decrement forms
(#18120).
