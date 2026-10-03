# Algorithm::Evolutionary::Simple passes its own tests

Drawing `Algorithm::Evolutionary::Simple` from the ecosystem roulette turned up
a cluster of binding and type-relationship bugs. All three of its tests that
pass under rakudo (`00-functions`, `01-basic`, `01-max-ones`) now pass under
mutsu too.

- A `Seq` is no longer a `List` or `Positional` (`(1,2).Seq ~~ List` is False,
  as in rakudo). It binds to an `@` parameter through `PositionalBindFailover`,
  and multi dispatch accepts it there. A `--> List(Seq)` return now coerces the
  returned `Seq`.
- An `@ is copy` parameter is a mutable Array when the argument is a `Seq`, a
  finite `gather`, or an itemized List read from a `$` variable or an Array
  element.
- An `IntStr` allomorph, or any Int with a mixin, satisfies `UInt`.
- A typed `MixHash`/`BagHash`/`SetHash` scalar that still holds its type object
  keeps the element object of the first key stored into it. A slice store
  (`$m{@keys} = ...`) also autovivifies it now, instead of failing the element
  type check against `MixHash`.
- Passing a `$` variable to a plain `@` parameter no longer writes the
  de-itemized List back over the caller's variable. Before this fix, `$q` then
  acted as a slice key (`%h{$q}`).
