# `is repr(CONSTANT)` resolves the constant

`class xmlDict is repr(Opaque)` where `constant Opaque = 'CPointer'` left the
class `P6opaque`, so a NativeCall routine returning it died with "Unknown type
... used in native call". The representation name is now looked up as a
constant when it is not itself a representation. LibXML's `t/00dict.t` loads
past `LibXML::Raw::Dict`.

Binding an element (`$obj<k> := $v`) of an object that defines `ASSIGN-KEY`
but no `BIND-KEY` now dies `X::Bind`, as Rakudo does, instead of silently
storing through `ASSIGN-KEY`.
