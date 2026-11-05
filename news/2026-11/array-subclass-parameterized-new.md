# `Foo[Str]` on a user subclass of Array is an Array of that element type

`class P is Array { }; P[Str].new("a", "b", "c")` was an object with none of Array's behaviour
(`.elems` was 1, `.Int` died, `.of` was missing): `P[Str]` was only a package named `"P[Str]"`.
`Foo[T].new(...)` now builds the class's own instance, checks the elements against `T`, tags the
backing array with the element type (so `.of` and later `push`/stores see it) and registers `Foo[T]`
as a class whose parent is `Foo`, so `.^name`, `.WHAT`, `~~ Foo` and the MRO match rakudo.

`my Str @t is Foo` declares a `Foo[Str]` the same way (it built a plain `Foo` and ignored the
declared element type); an untyped `my @u is Foo` stays a plain `Foo`.

Found by Array::Rounded (`t/01-basic.rakutest`). Test: `t/routines/signature/array-subclass-parameterized-new.t`.
Not done: `Foo.^mixin(Array::Typed[T])` and a user `method ^parameterize` for the `is` trait
(items 2 and the user-method half of 3 in #16658).
