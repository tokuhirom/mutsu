# `Nil.keys` and friends are the empty List

`Nil.keys` printed `Nil` and `Nil.keys.sort` printed `(Nil)`; rakudo prints
`()` for both (issue #9127). The same was true of `.values`, `.kv`, `.pairs`,
`.antipairs` and `.invert`. It surfaced wherever a failed match or parse was
keyed, e.g. `H.parse('x', :rule<t>).keys.sort`.

Two layers were wrong. The `MethodCall` opcode's Nil pre-dispatch treated
these methods as ones `Nil` does not define, so `Nil.FALLBACK` absorbed them
and returned `Nil`. They are in fact `Any` methods with an `Any:U` candidate
(`multi method keys(Any:U:) { () }`), so they now fall through to normal
dispatch, and `nil_absorbs_method` (which the hyper leaf consults) lists them
too. Below that, the native 0-arg methods answered a Nil invocant for only
three of the six, and with an empty `Seq` rather than a `List`; one guard
arm now returns the empty List for all six, so a Nil bound to a named
container (`my $x := Nil; $x.keys`) gets the same answer.

Pinned by `t/types/nil-key-value-views.t`.
