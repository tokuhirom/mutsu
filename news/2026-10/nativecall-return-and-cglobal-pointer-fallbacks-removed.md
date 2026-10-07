# NativeCall: provider-era Pointer construction in call returns and `cglobal` removed

A pointer-shaped `nqp::nativecall` return, including `Pointer[T]`, now hands back the bare
address and is boxed as `$rettype` through upstream's own types. `cglobal` of a pointer target
no longer falls back to building the retired provider's `Pointer` instance. `make_typed_pointer`
and `make_pointer_value` are deleted. `t/nativecall` and `t/modules` pass unchanged.

Part of [#11203](https://github.com/tokuhirom/mutsu/issues/11203); the callback-argument
`Pointer` and the name-keyed `CArray` constructor branches remain.
