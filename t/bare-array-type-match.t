use v6;
use Test;
use NativeCall;

# Parameterization narrows a type; it does not replace it. A `array[T]` is-a
# `array`, so the unparameterized spelling — which is what
# `NativeHelpers::Blob` uses in `multi sub pointer-to(array:D \arr, ...)` —
# must accept it. See todo/deep/nativehelpers-blob-moarvm-guts.md Gap A: the
# bare `array` constraint previously matched no value at all (unlike bare
# `CArray`, which already special-cased this), so `pointer-to($native_array)`
# could never select the right multi candidate.

plan 9;

my array[uint8] $a .= new(1, 2, 3, 4);

ok $a ~~ array, 'a native array[T] instance smartmatches the bare array';
ok $a ~~ array[uint8], 'and its own parameterization';
nok $a ~~ array[int16], 'but not a different parameterization';
nok 42 ~~ array, 'a non-array is not an array';
nok [1, 2] ~~ array, 'a plain Array is NOT an array (raku parity)';

# The type-object level, too.
ok array[uint8] ~~ array, 'array[T] the type object is-a array';
nok array ~~ array[uint8], 'the relation is not symmetric';

# A `array:D` parameter must bind a native array[T]. This is the exact
# signature shape `NativeHelpers::Blob`'s `pointer-to` multi uses.
sub takes-array(array:D \arr) { arr.WHAT.^name }
is takes-array($a), 'array[uint8]', 'a array[T] binds a bare array:D parameter';

# The multi-dispatch case this all exists for: NativeHelpers::Blob's
# `pointer-to(array:D \arr, :$typed)` overload must now actually be selected
# (it used to die with "none of these signatures matches" because the bare
# `array` constraint matched nothing).
use lib 'modules/NativeHelpers-Blob/lib';
use NativeHelpers::Blob;
my $p = pointer-to($a);
ok $p.defined && +$p != 0, 'pointer-to(array:D) selects the array candidate';

# vim: expandtab shiftwidth=4
