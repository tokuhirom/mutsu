# Multi-token start rules and enum-keyed Pair subscripts

Working CSS::Specification's `t/defs.t` (0 to 47/47 assertions, at parity with rakudo) exposed two
gaps. `Grammar.subparse(:rule<name>)` on a name with several `multi token` candidates evaluated
every candidate against the (empty) start arguments, so the one-parameter candidate died with
"Too few positionals" before the zero-argument one could run; a candidate whose signature cannot
bind is now skipped, and the bind error is reported only when no candidate applies. And
`$pair{ENUM}` on a Pair whose key is a `Str` enum value compared the enum's name rather than its
string value, so `Pair{enum}` returned Nil while `:exists` said True; it now subscripts by the
string form like a Str key.
