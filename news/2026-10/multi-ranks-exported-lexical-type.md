# Multi dispatch ranks an exported candidate's lexical type even when the caller never imported it

A module that exports a multi typed with a lexical class it imported
(`my multi sub g(VL:D $_)`, where `VL` is a `my class` handed over by another
module's `EXPORT` sub) lost the narrowness ranking to an `@_` candidate when
the calling script had not imported `VL` itself: the candidate was found
applicable, but its type constraint was ranked in the caller's scope, where
the bare name resolves to nothing and scored as unrelated, so `Positional`
looked narrower.

The ranking now resolves such a name through the registry's unique lexical
type key, the same resolution the applicability check already uses, so a
`VL` argument picks the `VL` candidate as in Rakudo. This makes
`immutable` 0.0.3's `t/01-basic.rakutest` test 12 (`immutable(immutable @a)`)
pass (#12710).
