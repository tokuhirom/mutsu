# Type captures, `C[T]` signature types and role REPRs answer real type objects

Three introspection gaps blocked upstream NativeCall's `check_routine_sanity`
when `use NativeCall` loads the vendored module (#11203). Each one made
`validnctype` warn "Not an accepted NativeCall type" for a valid parameter.

- A `::T` type capture of a role-mixed value now binds its composed type.
  This covers `C.^mixin(R)`, `$obj but R` and a `^parameterize` result like
  upstream's `CArray[uint8]`. The type comes from the same composition cache
  `.WHAT` uses. Before, the capture flattened to `Package` or `Any`.
- `C[T]` in a signature, where `C` declares its own `method ^parameterize`,
  is evaluated when the routine is declared, as rakudo does at compile time.
  `Parameter.type`, `Signature.returns` and `Routine.returns` then answer that
  type object instead of a bare name, so `.REPR` (`CArray`, `CPointer`) and
  the mixed-in role's methods are visible.
- A role type object reports `.REPR` as `Uninstantiable`. This covers
  `Blob`, `Positional[Int]`, `Buf[uint8]` and user roles. Before, it
  reported `P6opaque`.
