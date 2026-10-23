# Spaced and array regex variable aliases round-trip through RakuAST

`$foo = [<two>]`, `$foo= ATOM`, `$foo =ATOM` and `@x=ATOM` in a regex are aliases
to a variable, as `$foo=ATOM` already was: Rakudo keeps only the interpolation and
drops the aliased atom. The source regex tree now models all of those spellings
(the whitespace around the `=` and the array sigil are kept as written, so the
rebuilt text is the text that was written), so `.AST` no longer refuses a regex
literal that uses them. `t/regex/regex-variable-alias-drops-atom.t` now passes
under `MUTSU_RAKUAST=1`; fourteen files that already passed there joined the
frontend ratchet.
