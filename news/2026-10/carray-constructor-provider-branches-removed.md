# CArray: provider-era constructor branches removed

`CArray` is not a core type, so `Array`-style construction no longer special-cases the name
`CArray` (argument flattening, native-storage `make_carray`, the untyped-`CArray` metadata
tag). Upstream's `CArray[T]` is built by its own `is repr<CArray>` class and `^parameterize`.
`make_carray` is deleted. `t/nativecall`, `t/modules` and `t/types` pass unchanged.

Part of [#11203](https://github.com/tokuhirom/mutsu/issues/11203); the callback-argument
`Pointer` in `nativecall_callback.rs` is the last provider-era construction left.
