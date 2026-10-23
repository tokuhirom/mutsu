# `Attribute.type` of a parameterized class is the type object it denotes

`has CArray[uint8] $.a` reported a bare package named `NativeCall::Types::CArray[uint8]` as the
attribute's `.type`: not `===` to `CArray[uint8]`, without the composed `UIntTypedCArray` role, so
`.allocate` was missing. A class attribute whose type is `C[T]` for a `C` declaring `^parameterize`
now evaluates the type once, when the attribute is declared, and `.type` answers that type object.
`nativesizeof` accepts such mixin type objects too (`nativesizeof(CArray[uint8])` died). With both,
Notcurses::Native 0.7.0's `t/42-abi-guard.rakutest` runs to its 807 assertions like rakudo (#14672).
