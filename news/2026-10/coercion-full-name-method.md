# A coercion type calls the method named by the target's full name

Rakudo coerces into a type by calling the method named by that type's full
name. So `method NativeCall::Types::Pointer { ... }` answers a `Pointer()`
coercion, and `method M::T { ... }` answers `M::T()`. mutsu looked for a
method named by the target as written. That found neither when the written
name is an alias of another type (`Pointer` is a constant alias of
`NativeCall::Types::Pointer`). For a qualified name, it read the method name
as a qualified call (`$obj.M::T`, "Cannot dispatch to method T on M").

The coercion now tries the full name first, then the written name, and calls a
qualified method name literally through the VM's dispatch by name.
Font::FreeType 0.5.16's `t/01-face.t` (`my Pointer() $ = $face`) now passes,
as it does under `raku`.
