# A `VMArray` class with methods is a real instance, and `is IterationBuffer` subclasses are buffers

The `Tuple` / `ValueList` distributions declare `my class ValueList is IterationBuffer is repr('VMArray')`
with methods and build instances with `nqp::create(self)!SET-SELF: ...`. Three interpreter gaps
stopped `Tuple`'s suite at its first assertion:

- `nqp::create` of an `is repr('VMArray')` / `is repr('VMHash')` class always returned mutsu's bare
  array or hash, so a class that declares methods could not dispatch them
  (`Calling private method 'SET-SELF' must be fully qualified ...`). Only a class with no declared
  methods is a bare storage class now; any other is allocated as a real instance.
- Instances of an `IterationBuffer` subclass are seeded with element storage at creation and take
  the buffer methods (`elems`, `List`, `Slip`, `Seq`, `push`, `append`, `iterator`, ...) and the
  `nqp::` list ops like `IterationBuffer` itself.
- In a multi method, `(@args)` now beats `(+@args)` for the same argument instead of raising
  `X::Multi::Ambiguous`: a `+@` parameter is a slurpy and never counts as a Positional constraint.
- A `my @a is Foo` variable passed to a sigilless `\c` parameter is no longer rebuilt into a plain
  `Array` by the writeback.

`Tuple`'s baseline file goes from 0 to 23 of 27 assertions. The rest is tracked in #10260 (a user
`WHICH` is ignored by `Set` / `unique`) and #10261 (immutable positional container semantics).
