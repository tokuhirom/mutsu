use v6;
use Test;
use NativeCall;

# Parameterization narrows a type; it does not replace it. A `CArray[uint8]`
# is-a `CArray`, so the unparameterized spelling — which is what
# `NativeHelpers::Blob` uses in `isa-ok $au, CArray` and in the signature
# `sub carray-is-managed(CArray:D \arr)` — must accept it.

plan 9;

my $a = CArray[uint8].new;
$a[3] = 0;   # force allocation

ok $a ~~ CArray, 'a CArray[T] instance smartmatches the bare CArray';
ok $a ~~ CArray[uint8], 'and its own parameterization';
nok $a ~~ CArray[int16], 'but not a different parameterization';
nok 42 ~~ CArray, 'a non-array is not a CArray';
nok [1, 2] ~~ CArray, 'a plain Array is not a CArray';

# The type-object level, too.
ok CArray[uint8] ~~ CArray, 'CArray[T] the type object is-a CArray';
ok Array[Int] ~~ Array, 'the same rule holds for Array[T]';
nok CArray ~~ CArray[uint8], 'the relation is not symmetric';

# A `CArray:D` parameter must bind a parameterized CArray. (Only the tail of
# the name is checked: Rakudo reports the fully-qualified
# `NativeCall::Types::CArray[uint8]`.)
sub takes-carray(CArray:D \arr) { arr.WHAT.^name }
ok takes-carray($a).ends-with('CArray[uint8]'),
   'a CArray[T] binds a bare CArray:D parameter';

# vim: expandtab shiftwidth=4
