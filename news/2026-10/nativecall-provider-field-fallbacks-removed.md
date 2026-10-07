# NativeCall: provider-era struct-field fallbacks removed

With `use NativeCall` resolving to the vendored upstream module, a `CStruct` field
declared as `CArray[...]` or `Pointer[...]` always reads back through upstream's own types
(`is repr<CArray>`, `NativeCall::Types::Pointer`). The name-keyed `CArray` handle and the
`make_typed_pointer` fallback in `cstruct_layout.rs`, which only the retired native
provider reached, are deleted. `t/nativecall` passes unchanged (251 files).

Part of [#11203](https://github.com/tokuhirom/mutsu/issues/11203); the remaining provider-era
fallbacks (`value_carray` / `methods_aggregate_ctor` name-keyed `CArray` branches) stay open.
