# `.ACCEPTS` answers on every built-in receiver

`$x.ACCEPTS($topic)` used to work only on the plain scalars, buffers and a few collections: a `List`, a `Hash`, a `Date`, a
`DateTime`, a `Duration`, a `FatRat`, a `Capture`, a `Uni`, a `Seq`, a `Match` or a plain user object threw
`X::Method::NotFound`. They are now rows of the built-in method table on the owners Rakudo declares (ADR-11276 §9.108):
the numeric types, `Str`, `List`, `Map`, `Date`, `DateTime`, `Seq` and `IO::Path` answer the smartmatch, `Bool` is itself,
a `Uni` compares codepoints, a `Match` is itself, and `Any.ACCEPTS` is `self === topic`, so `\(1).ACCEPTS(\(1))` is `True`
and `\(1).ACCEPTS(1)` is `False`, as in Rakudo. The runtime's catch-all `Mu.ACCEPTS` arm is gone.
