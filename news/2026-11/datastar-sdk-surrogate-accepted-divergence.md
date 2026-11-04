# Datastar::SDK `t/02-signals` is an accepted divergence (a surrogate `Str`)

The test expects `to-json(chr(0xD800))` to die `X::AdHoc`. rakudo keeps the
surrogate in a `Str` and fails only when the string is encoded; mutsu raises
`X::InvalidCodepoint` when `chr(0xD800)` is built, which is what roast's
`ord_and_chr.t` and S15 specify and what ADR-12640 (rejected, #14416) settled.
The file is graded `accepted`, so it no longer counts as a mutsu gap; the rest
of Datastar::SDK (18 of 19 assertions in that file, every other file) matches
rakudo.
