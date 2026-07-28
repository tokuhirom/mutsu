# A `unit module` that references the builtin NativeCall `Pointer` prelude.
# The prelude class is spliced into this compunit, so before the `GLOBAL::`
# pinning it registered as `UnitModPrelude::Pointer` -- a different type from
# the builtin, which could not be parameterized.
unit module UnitModPrelude;

use NativeCall;

sub prelude-pointer() is export { Pointer }

sub prelude-pointer-of() is export {
    my \t = uint8;
    Pointer[t];
}

sub prelude-void() is export { void }

# The `Rational` role prelude is spliced in the same way.
class UnitRat does Rational[Int, Int] is export { }
