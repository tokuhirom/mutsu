# `CORE::v6c::` / `CORE::v6e::` name one revision's setting

`CORE::v6c::CORE-SETTING-REV` (and the `v6d`, `v6e` forms) is now that revision's letter.
`CORE::v6e::PseudoStash` is 6.e's `PseudoStash` class, the one core class 6.e redefines.
`CORE::v6c::PseudoStash` is 6.c's, which 6.d inherits. `.^ver` tells them apart, and so does
a stash built by code of either revision. A signature typed with one of them accepts only
that revision's stashes, so a multi with a `CORE::v6c::PseudoStash` candidate and a
`CORE::v6e::PseudoStash` candidate dispatches by the module that built the stash. Any other
`CORE::v6X::Name` is the core `Name`.

This also fixes a routine identity bug. Two modules that each exported a same-named sub with
an identical body (roast's `Pseudo6c` and `Pseudo6e` both declare
`sub pseudo-stash is export { MY:: }`) were taken for one declaration. The second
module's registration was skipped as a re-run of the first, so every block's import called
the first module's routine. The re-registration check now also requires the same source file.

`S02-names/pseudo-6e.t` now runs to its end (#18742).
