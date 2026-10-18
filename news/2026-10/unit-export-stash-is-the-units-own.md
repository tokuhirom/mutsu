# A unit's `EXPORT` stash lists only its own exports

`use Test; sub mine is export { 1 }; say EXPORT::DEFAULT::.keys` now prints
`(&mine)`, as in rakudo. The stash also listed every `is export` routine of
Test.rakumod, because a package-less module's exports land in the same table as
the program's own; they are now left out, except to the module's own code (its
`sub EXPORT` reading `EXPORT::all::`). `MY::<EXPORT>` exists at the root of a
unit that exports a routine, and not in a block or routine inside it (#13854).
