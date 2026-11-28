# `exit` with a caller-installed `&*EXIT` hook leaves the routine

When a `&*EXIT` hook is in scope, `exit` calls it with the status instead of
ending the process. mutsu then returned normally from `exit` and ran the rest of
the routine; rakudo 2026.11 instead leaves the enclosing block, so the routine
returns `Nil` and, at the mainline, the rest of the unit is skipped with status 0.
`exit` now raises the same leave signal as `leave` after the hook returns.
`Test`'s `exits-ok` keeps working, since it relies only on the hook being called.
