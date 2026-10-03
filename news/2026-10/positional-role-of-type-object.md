# `.of` follows a composed `Positional[T]` on type objects and through roles

`class A does Positional[Int]; A.of` died with "No such method 'of'": mutsu
answered `.of` from a composed container role only for an instance, and only
when the class named `Positional[...]`/`Associative[...]` itself. Upstream
NativeCall's `CArray[uint8].of` takes the remaining shape: `^parameterize`
mixes in `IntTypedCArray[uint8]`, a role that `does Positional[TValue]`, and
`check_routine_sanity` reads `.of` off that type object to validate every
`CArray[...]` parameter of an `is native` routine (#11726, part of #11203).

`.of` now finds the container role on a class type object, on a `.^mixin`
type object, and through a parametric role that passes its own type parameter
on (`role R[::T] does Positional[T]`; `class B does R[Str]` answers `Str`).
