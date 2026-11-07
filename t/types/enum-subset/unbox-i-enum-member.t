# An integer enum member unboxes to its value: `nqp::unbox_i(D)` was 0, so
# `CArray[int32].new: FFTW_REDFT00, ...` held zeros (Math::FFT::Libfftw3 planned the wrong
# r2r transform, #17077).
use Test;
use nqp;
use NativeCall;
plan 9;

enum E <A B C D>;
enum F (X => 5, Y => -2);

is nqp::unbox_i(D), 3, 'unbox_i of an enum member';
is nqp::unbox_u(D), 3, 'unbox_u agrees';
is nqp::unbox_i(Y), -2, 'a negative member';
is nqp::unbox_i(X), 5, 'an explicit value';

is-deeply CArray[int32].new(D, D).list, (3, 3), 'CArray[int32].new of enum members';
is-deeply CArray[int64].new((C, D)).list, (2, 3), 'CArray[int64] from a list';
is-deeply CArray[uint8].new(C, D).list, (2, 3), 'CArray[uint8] (already worked)';

my @a := CArray[int32].new;
@a[0] = C;
is @a[0], 2, 'ASSIGN-POS of an enum member';
@a[1] = Y;
is @a[1], -2, 'ASSIGN-POS of a negative member';
