# A `::T` capture bound to a parameterized type is not shadowed by a class named `T`

`sub name-of(Mu ::T) { T.^name }; class T { }; name-of(R[Int])` answered `T` (the program's class)
instead of `R[Int]`; the same for `CArray[int32]` / `Pointer[int32]`. A capture bound to a plain
type already won over a same-named class, but one bound to a role specialization or a
class-with-role-mixin parameterization fell through to the type registry. NativeCall's own
`validnctype(Mu ::T)` helper therefore warned "Not an accepted NativeCall type" for every
`CArray[...]` parameter in a program that declared `class T`, and Math::FFT::Libfftw3's attribute
class sets lookups through the same path.
