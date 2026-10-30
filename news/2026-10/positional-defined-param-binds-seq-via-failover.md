# `Positional:D` binds a Seq, and a Seq bound to `Positional` stays reusable

A `Seq` is not `Positional`, but it composes `PositionalBindFailover`, which a `Positional` parameter
binds through `.cache`. The `:D` / `:_` smiley forms were missing from that rule, so
`sub f(Positional:D $p)` rejected `(1, 2).map({ $_ })`; YAMLish's `multi to-yaml(Positional:D $d, ...)`
then fell through to the "Can't serialize an object of type Seq" catch-all and `cro`'s
`CroFile.to-yaml` (a `.map` result inside the dumped hash) died. A plain Seq bound to a
`Positional`-typed parameter now also binds through `.cache` like an `@` parameter, so the same Seq
variable binds a second time instead of dying with "The iterator of this Seq is already in use".
`cro`'s `t/tools-crofile.rakutest` and `t/tools-link-editor.rakutest` pass.
