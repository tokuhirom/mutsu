# A lowercase-named module is a BEGIN-time `use`, not a pragma

The BEGIN prologue (ADR-0134) treated every lowercase `use` name as a positional pragma and left it
at its run-time position, so `use vars @vars;` (the real `vars` module, whose `sub EXPORT` installs
`$frob`/`@mung`/`%seen`) ran *after* a later `BEGIN ::{$_}:exists` and the symbols were missing.
Only the pragmas mutsu applies as positional run-time state, plus a language version, now stay in
place; any other lowercase name is an ordinary module load and moves into the prologue. Found
working the `vars` distribution, whose `t/01-basic.t` now passes 7/7.
