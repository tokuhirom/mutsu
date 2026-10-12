# `so`, `not`, `is-lazy`, `join` and `lazy` reach every receiver through the method table

mutsu answered `so`, `not`, `is-lazy`, `lazy` and `join` with name-matching
cascade arms that ran for every receiver. Those arms are gone, and with them
`methods_0arg/dispatch_core_str.rs`.

`Mu.so`, `Mu.not` and `Mu.is-lazy` are now rows that every receiver reaches:
values, type objects, closed shapes such as a `Pair` or a `Set`, code objects,
enum values and instances. Calling `so` or `not` on a `Failure` still marks it
handled. `lazy` stays the `Iterable` method Rakudo declares, so `5.lazy` is still
`X::Method::NotFound`. A lazy list keeps its tag when it is assigned to an array.

`Any.join` reads any receiver as its list, as Rakudo's `self.list.join` does.
Several answers moved to Rakudo's:

- an instance joins its own `Str` (`class C is Cool { method Str { "mc" } }`
  gives `C.new.join` as `"mc"`; it was `"C<42>"`);
- a type object is an empty string with the usual warning (`Int.join` was `"(Int)"`);
- a `Blob` joins its elements (`Blob.new(1, 2).join(",")` is `"1,2"`);
- a `Match` joins its positional captures for both arities.
