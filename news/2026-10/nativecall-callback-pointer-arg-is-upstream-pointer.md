# NativeCall: a callback's pointer argument is upstream's `Pointer`

A pointer-shaped parameter of a native callback (`&cmp (Pointer, Pointer --> int32)`) used to
arrive as an instance of a class named `Pointer` that no longer exists once `use NativeCall`
loads the vendored module, so `.Int` died with "No such method 'Int' for invocant of type
'Any'". It is now boxed as `NativeCall::Types::Pointer`, as rakudo hands it. This removes the
last provider-era `Pointer` construction (`make_pointer_object`) from the marshaller.

Part of [#11203](https://github.com/tokuhirom/mutsu/issues/11203).
