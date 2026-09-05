# RakuAST type-object `.^methods` and `.^lookup`

Four metaobject operations on a RakuAST model class disagreed with each other.
`.^methods(:local)`, `.^can` and `.^method_table` all answered from the model
metadata and found `RakuAST::IntLiteral`'s `value` accessor; `.^methods` (the
default, no adverb) returned `()` and `.^lookup("value")` returned `(Mu)`.

RakuAST model classes are native type objects with no entry in mutsu's class
registry, so each metaobject operation has to consult that metadata explicitly
or fall through to an empty registry answer. Two of them never did.

## Change

`src/rakuast/mod.rs` gained `inherited_method_names`, the MRO-walking companion
to `local_method_names`: it unions the model methods of a class and its model
ancestors, dropping the `Any`/`Mu` tail. Today the abstract RakuAST classes
(`Node`, `Expression`, `Term`, `Statement`) declare no model methods of their
own, so the result equals the local set — but it is the rule
`Type/Metamodel/MethodContainer.rakudoc` specifies, and it stays correct as that
metadata grows.

Three call sites now use it:

- `.^methods` with no adverb returns the inherited set. `:local` keeps the
  narrower `local_method_names` answer, and `:all` seeds the model names and
  then falls through to the ordinary built-in list, so the Any/Mu tail is added
  on top rather than replacing the model methods.
- `.^lookup($name)` returns the `Method` object when the name is in that set and
  `(Mu)` otherwise, matching the documented "first matching Method along the
  MRO, else `(Mu)`".
- `.^can` moves from the class's own names to the inherited set, so an
  inherited model method is found, exactly as `.^can` on an ordinary class
  spans its MRO.

## Coverage

`t/rakuast-type-lookup.t` (11 assertions) pins all three adverb cases of
`.^methods`, `.^lookup` on a present and a missing method (including that it
returns a `Method`), `.^can` agreeing with both, and the three staying in
lockstep on a second class.

It is deliberately mutsu-only rather than dual-oracle: the method *names* are
mutsu's own model API — `local_method_names` documents them as describing
mutsu's implemented model rather than Rakudo's compiler-internal `IMPL-*`
surface — so a raku run would legitimately report a different set.
