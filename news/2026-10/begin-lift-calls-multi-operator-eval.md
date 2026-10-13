# A nested BEGIN can call a multi, an `our` sub or an operator of its scope, and EVAL its names

A BEGIN nested in a routine runs at compile time (ADR-0134), but only when the
unit prologue can give it what it uses. A BEGIN calling a `multi`, an
`our sub`, an exported sub or an operator declared ahead of it in its scope
used to stay on the old path, so `sub f { multi sub h(Int) { 1 }; BEGIN say h(5) }`
printed nothing unless `f` was called.

The lifted body's block now repeats those routines too: every multi candidate
and proto a call can reach, an `our` or exported sub as a lexical-only copy,
and every operator of the scope (whose syntax the scan cannot see). A body that
uses `EVAL`, `.EVAL` or a symbolic lookup gets every variable and routine of
its scopes, so `sub f { my $x; BEGIN EVAL 'say $x' }` now prints `(Any)` as on
rakudo, where it used to fail at startup with `Variable '$x' is not declared`
(#10395).
