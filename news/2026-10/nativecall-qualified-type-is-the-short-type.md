# A NativeCall type's qualified spelling is the same type as its short one

`use NativeCall` imports `CArray`, `Pointer`, `size_t` and friends as aliases for the
types declared in `NativeCall::Types`, so `CArray === NativeCall::Types::CArray` is
`True` in rakudo. In mutsu it was `False`, and so were `eqv`, `.WHICH` and an object-hash
lookup, because the bareword `NativeCall::Types::CArray` pushed a type object named after
its own spelling while `CArray` pushed one named after the registry key (ADR-0056 keeps
that key bare and qualifies it only for display).

The qualified spelling now resolves to the registry key where the type object is built,
so the two spellings are one value everywhere: `===`, `eqv`, `=:=`, `.WHICH`, object-hash
keys and `~~`. Display is unchanged (`.^name` and `.raku` still print the qualified name),
no identity or dispatch comparison was touched, and a program that declares its own type
under a NativeCall name (`class void { }`) keeps meaning its own. Two spellings that
never resolved before work too: `NativeCall::Types::CArray[int32].new(...)` and
`NativeCall::Types::size_t` (a lowercase type name was taken for a sub call).
