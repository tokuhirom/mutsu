# `for` iterates an object whose class declares only `list`

`for WithList.new { ... }` now runs once per element of the object's own
`list`, as in Rakudo. Rakudo's `for` is `.map`, `.map` reads
`self.iterator`, and `Any.iterator` is `self.list.iterator`, so no
`Iterable` role is needed. mutsu used to run the loop once, over the object
itself, unless the class composed `Iterable` (#13748). A Scalar-held object
and list assignment still treat the object as one item, as Rakudo does.
