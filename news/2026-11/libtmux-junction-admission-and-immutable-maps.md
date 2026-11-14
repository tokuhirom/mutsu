# LibTmux: junction admission, immutable Maps and Promise.Supply

LibTmux's test suite now matches rakudo in every file but one (`t/unit/query`,
which needs a distinct utf8-c8 synthetic grapheme, #17800). The library guards
each effectful method with an `is single-operation` trait that wraps the method
and rejects Junction arguments before typed binding, and exposes its captured
state through immutable `Map`s and `List`s. Getting there fixed:

- `$j.^isa(Junction)` and the other `.^` metamethods no longer thread over a
  junction invocant, and a built-in HOW takes a junction argument whole.
- A wrapped method binds its arguments to the outermost wrapper's signature
  first, so a `|capture` wrapper sees a junction (positional or by any alias
  name) whole; the wrapper's `callsame` then threads at the original signature.
- `self!m(...)` runs the wrapper chain of a wrapped private method.
- A role's `proto method` body sees the routines its module imports.
- Every route to an element of an immutable `Map` refuses the store: through an
  accessor, an expression, or a chained subscript into a nested Map; a Blob
  element is immutable too, and `Map.new` keeps a Blob value whole.
- `$list.grep(..., :kv)` no longer makes an immutable List's elements writable.
- A metamethod pulls nothing from a lazy List, and a gather's List view passed
  as a named argument is no longer run to completion before the call.
- `Promise.Supply` emits the result and is done (or quits with the cause), also
  for a promise resolved after the tap and under a react `whenever`.
- An anonymous `$` parameter no longer declares a phantom `state` variable in
  the enclosing routine.
