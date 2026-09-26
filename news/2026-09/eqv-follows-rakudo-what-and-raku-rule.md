# `eqv` on user objects follows Rakudo's `.WHAT` + `.raku` rule

For two defined objects, Rakudo's fallback `infix:<eqv>(Any:D, Any:D)` answers
True when they are the same object, or when they have the same `.WHAT` and
equal `.raku` strings. mutsu compared user instances attribute by attribute
instead, so three cases answered False where Rakudo answers True
([#9591](https://github.com/tokuhirom/mutsu/issues/9591)):

- objects that differ only in a **private** attribute (the default `.raku`
  renders public attributes only);
- objects of a class with its own `raku` method, whose output does not show
  the differing attribute;
- a punned role (`R.new`) with its own `raku`.

`Value::eqv` stays a pure structural walk, but it now takes an
`EqvInstanceHook` that it consults for every same-class user-instance pair,
including pairs nested inside arrays, hashes and other objects. The VM's
`eqv` (and so `is-deeply`) passes a hook that calls a user `raku` through the
compiled method dispatch, with no slow-path route, and otherwise leaves private
attributes out of the comparison. An exception thrown by a user `raku`
propagates out of `eqv`.

A related fix came out of the Array::Sparse test that found the issue: a
method call on a punned role instance left a role-qualified copy of an
attribute in the instance. `eqv` now ignores a role-qualified slot that equals
its bare slot, as it already did for class-qualified slots, so the call no
longer changes the answer.

Together with the `ASSIGN-KEY`-through-an-alias fix that landed separately,
Array::Sparse 0.0.13's `is-deeply @a.raku.EVAL, @a` round-trip passes.
