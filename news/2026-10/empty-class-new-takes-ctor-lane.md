# `.new` of a class with no attributes takes the constructor lane

A class the program declares with no attributes (`class E { }`, or one with
only methods) did not count as having a fully known attribute set. The guard
was meant for the built-in `X::` exception types, which are registered with an
empty attribute list and used as attribute bags, and it caught user classes
too. The constructor lane therefore never served such a class, and every
`E.new` walked the whole method-dispatch chain: about 25 µs per call on a
release build, against about 2.5 µs for a class with one attribute.

A class the program declared is now known to have exactly the attributes it
declares, even when that is none. `E.new` takes the lane, and the default
constructor ignores a named argument that names no attribute, as rakudo does:
`E.new(foo => 1) eqv E.new` is now `True`.
