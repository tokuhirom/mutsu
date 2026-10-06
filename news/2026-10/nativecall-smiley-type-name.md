# A NativeCall type renders qualified under a definiteness smiley

`(CArray:D).raku` and `(CArray:D).^name` printed the imported short spelling `CArray:D`
while `CArray.raku` printed `NativeCall::Types::CArray`; Rakudo prints
`NativeCall::Types::CArray:D` for both. The NativeCall qualification of a type's display
name (`value/display.rs`) matched the bare name and its `[T]` parametrisation only, so a
`:D` / `:U` smiley hid the type from it. The smiley is now peeled off, the type qualified
and the smiley put back, for `CArray`, `Pointer`, `size_t` and the other NativeCall names;
core types (`Int:D`) and native integers (`int32:D`) are unchanged (#11871).

Two neighbouring gaps found on the way are filed separately: a signature still renders
its parameter's NativeCall type by its short name (#12030), and `CArray` is not `===`
`NativeCall::Types::CArray` (#12031).
