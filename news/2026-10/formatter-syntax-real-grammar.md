# `Formatter::Syntax` is a real grammar, and `Format`/`Formatter` are 6.e-only

`Formatter::Syntax` is now a `Grammar` written in Raku (a prelude compiled on
first use, with Rakudo's rule names and captures: `statement`, `directive`,
`literal`, `panic`, `idx`, `flags`, `size`, `precision`), so `parse` and
`subparse` are `Grammar`'s own, an invalid format string no longer "parses" and
the by-name `Match` stub is gone. `Format`, `Formatter` and `Formatter::Syntax`
are undeclared names below language revision 6.e, as in Rakudo.
