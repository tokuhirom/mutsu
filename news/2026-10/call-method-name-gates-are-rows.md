# The interpreter entry's by-name method gates are rows

About twenty `method == "..."` gates in front of the built-in method table in
`call_method_with_values` are gone (ADR-11276 §9.164). `ACCEPTS` of a regex, a
code object, a `WhateverCode`, a `Signature` and a type object, `Any`'s growing
methods on an undefined receiver, `Range`'s refused mutators, `Mu.WALK`,
`Mu.return`, `Mu.rakuseen`, `List.from-iterator`, `Match.make` and `Seq.Int`
are method-table rows of the types Rakudo declares them on, so `.^can` and the
dispatch agree; the VM's two copies of the `Any:U` autovivification go with
them.

Behaviour now matching Rakudo: `5.self(1, 2)` dies on the positional
arguments, `Seq.from-iterator` is no method, and `.chomp` of an `IO::Handle`
subclass with no handle answers its default.
