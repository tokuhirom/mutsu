# `Array::Typed` and parametric-role mixins into Array subclasses

`Foo.^mixin(Array::Typed[Str])` now works for a user subclass of `Array`: `Array::Typed` is a
real parametric role (declared on demand, answering `.of`), `Base.^mixin(R[T])` registers the
`Base+{R[T]}` class so `.new` keeps the Array constructor, `^set_name` on the mixin type renames
its instances too, and `my Str @t is Foo` builds `Foo[Str]` through the class's own
`^parameterize` (#16658). `Array::Typed[Str].^roles` still shows `Positional[TValue]` instead of
`Positional[Str]`.
