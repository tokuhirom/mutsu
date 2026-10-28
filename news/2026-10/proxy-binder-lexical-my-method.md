# annotations' Binder: `my method` lexicals, bound attributes and Proxy assignment

annotations 0.1.0's `Binder is Proxy` passes `:&FETCH, :&STORE` (two `my method`
of the class body) to `callwith`. Four gaps stood in the way, each fixed on its
own terms:

- A class body's `my method` is lexical to the body: sibling methods now reach it
  by name and as `&name` (the registry short name is dropped at body exit, so the
  routine rides the closure env like a body `my sub`).
- `$!attr := VALUE` binding a plain value ends the attribute being a writable
  container, remembered on the instance, so `nqp::isrwcont($!attr)` in a later
  method call answers false.
- `($proxy = v)` is the Proxy container, so a reader FETCHes it; it used to be
  the assigned value.
- `Type.new = v` on a Proxy subclass runs the class's own `method new` (it was
  handed to the native Proxy constructor).
