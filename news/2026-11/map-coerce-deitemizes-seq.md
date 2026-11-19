# `.Map` stores a Seq value bare

`.Map` on a list of Pairs, `my %m is Map = ...` and a `--> Map()` return
coercion fold their input through a Hash, which itemizes each value, and then
decontainerize the values again. That decont (`Value::deitemize_element`, the
inverse of the element-store itemization) undid an itemized Array, Hash, Slip
and Scalar, but not a Seq's `$` view, so `(a => $seq,).Map<a>` stayed
`$((1, 2, 3).Seq)`.

Since a `$`-held Seq now stays one element in an `@` container (#18193),
`C.new(|$map)` then bound `has Int @.a` to the Seq itself and died with
"Type check failed for an element of @!a; expected Int but got Seq". This is
the RedFactory `t/03-not-model.rakutest` regression of nightly sweep #109: its
`args-for` builds the constructor arguments with `--> Map()`.
`deitemize_element` now maps a Seq's `ItemSeq`/`ItemList` view back to the
bare handle over the same reification core (#18256).
