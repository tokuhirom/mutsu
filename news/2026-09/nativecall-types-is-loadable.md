`NativeCall::Types` is now recognized as a loadable NativeCall provider. Code
that only needs NativeCall's type declarations can use the submodule directly,
including `Pointer`, `CArray`, `int32`, and `size_t`.
