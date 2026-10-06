# A signature's `.raku` qualifies a NativeCall parameter type

`sub f(CArray $x)` renders its signature as `:(NativeCall::Types::CArray $x)` in
rakudo; mutsu wrote `:(CArray $x)`. A NativeCall type object's own rendering was
already qualified (`user_facing_type_name`), but the signature renderer wrote each
parameter's type constraint as the source spelled it.

`render_param` and the return-type slot of `render_signature` (behind
`Signature.raku` and `.gist`) and `parameter_to_raku` (`Parameter.raku`) now pass
the type through `user_facing_type_name`, which keeps a definiteness smiley and a
`[T]` parametrization (`NativeCall::Types::Pointer:D`,
`NativeCall::Types::CArray[int32]`, and a qualified type parameter such as
`Pointer[NativeCall::Types::void]`). Core types, user classes and native
primitives such as `int32` render as before.
