# NativeCall: provider-era Pointer boxing fallbacks removed

`nqp::box_i` into a `Pointer`/`Pointer[T]` target and `nativecast` to `Pointer[T]` now only
box through upstream's own types (`native_object_of_type`). The name-keyed branches that built
the retired provider's `Pointer` instance with an `of` attribute are deleted. `t/nativecall` and
`t/modules` pass unchanged.

Part of [#11203](https://github.com/tokuhirom/mutsu/issues/11203).
