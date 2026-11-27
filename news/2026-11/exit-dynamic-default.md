# `&*EXIT` is bound to `exit` by default

Rakudo's process-level `&*EXIT` is the `exit` routine itself, and zef's
`MAIN` `CATCH` ends every failed command with `&*EXIT(1)`. mutsu resolved an
unbound `&*EXIT` to a routine named `EXIT`, so a failed `mzef install` died
with `Unknown function: EXIT` instead of exiting with status 1. An unbound
`&*EXIT` now resolves to `exit` (`&*EXIT.name` is `exit`); a caller's own
`my &*EXIT` hook still wins (#19247).
