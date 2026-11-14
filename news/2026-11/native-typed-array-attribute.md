# A native-typed `@` attribute is an `array[T]`

`has uint32 @!to` is a native `array[uint32]` on rakudo, as `my uint32 @a`
is: a gap an out-of-range store grows reads `0`. mutsu built every typed
`@` attribute as an `Array[T]`. Its gaps read `Any`, so Font::FreeType's
`@!to-unicode[self.num-glyphs] = 0` left `Any` holes. Binding the result
to PDF::Font::Loader's `has uint32 @.unicode-index` then failed the
element type check, and `$obj.c[4] = 9` on a `has int @.c` died with
"cannot unbox".

Every attribute container builder (default construction, the fast native
constructor, the lazily tagging accessor read, the typed-container
finalizer) now tags a native element type as `array[T]`. An element store
on the attribute fills its gap from the container's own element type, and
the accessor element store keeps the container's type metadata. A boxed
array's gap reached through the accessor still reads as present; that is
#17778. Test: `t/oo/attribute/attribute-int-array-gap-zero.t`.
