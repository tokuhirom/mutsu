# `Any`'s list coercions answer from the method table

`.List`, `.list`, `.Array`, `.Slip` and `.Supply` of a receiver with no row of its own (a `Range`, an unsettled `Seq`, a
buffer, a lazy list, a `Supply`, a type object), `Mu.Capture`, and the type objects' `Pair`, `Complex` and `isNaN` are now rows
of the built-in method table; the name-matching arms in `methods_0arg/coercion.rs` that answered them are gone (ADR-11276
§9.110). A `Capture` coerces its positionals (`\(1, 2, :a(3)).Array` is `[1 2]`), `Any.Capture` is `\()`, `Int.Capture` is
`X::Cannot::Capture`, and `Str` has no `isNaN` and no `Range`, as in Rakudo.
