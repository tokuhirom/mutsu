# `UNIT::` in a module's `sub EXPORT` sees the module's own scope

A module's `sub EXPORT` often picks what to export by reading `UNIT::`, the
compunit's own lexical scope. path-utils does
`UNIT::{"&$_"}:exists` for each requested name, and its names are
`my constant &path-exists = -> ... { ... }`.

In mutsu, `UNIT::` was built from the running frame's env, and on a call frame
iterating that env sees only the frame's own overlay. `EXPORT` runs over the
module scope env, and a closure inside it runs over the `EXPORT` frame, so the
module's own `my constant &name`, `my $x` and the like were never listed (#13272).
`UNIT::` now also lists the env the chain bottoms out at, which is the module
scope for an `EXPORT` and the program scope for a script's routine.

With this, `use path-utils 'path-is-directory:isdir'` (a renaming selective
import) works, as it does in rakudo. path-utils' `t/02-selective-importing`
stays at 9 of 43 because of a separate `MY::` problem in bare blocks (#13353).
