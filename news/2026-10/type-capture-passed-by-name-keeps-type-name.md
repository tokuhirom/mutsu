# A `::T` capture passed on by name resolves to the type's own name

`self.add-it(T, $n)` — handing a `::T` capture on to another `::T` method by its bare name — leaves the
callee's `T` in a shared cell. Resolving a `T:U:` invocant constraint read that cell's display string,
which equals the type's name for a named class but is `<anon|N>` for an anonymous one (registry name
`__ANON_CLASS_N__`), so a method literal `method (T:U:) { ... }` rejected the very type it was added to
with "Invocant of method 'a' must be a type object of type 'T', not an object instance". The capture is
now taken out of its cell before its Package name is read. This is the `Red::Model.add-column` ->
`Comparate.add-comparate-methods` path of anonymous `model { ... }` types (Red's `t/01-tdd`).
