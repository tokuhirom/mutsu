# Smartmatch agrees with Rakudo's ACCEPTS for Duration, Complex, Map and Blob

A `Duration` topic or matcher now compares by numeric value against a `Numeric`,
a numeric `Str` matches a `Complex`, a `Hash` and a `Map` no longer match each
other, and a `Blob` topic against a `Str` dies `X::Buf::AsStr` (also through
`Str.ACCEPTS`, which now propagates exceptions raised while matching). The
remaining item of #13332, `Seq` consumption by `ACCEPTS`, is left open.
