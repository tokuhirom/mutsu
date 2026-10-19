use Test;
use NativeCall;

# `CArray[T].allocate(n)` is upstream NativeCall's own method
# (`NativeCall::Types`): it creates the array and binds its last slot, so the
# array has n zeroed elements (ADR-11276 §9.193; checked against Rakudo).

plan 6;

my $a = CArray[int32].allocate(4);
is $a.elems, 4, 'an int32 CArray of 4 elements';
is $a.list, (0, 0, 0, 0), '... all zero';

my $b = CArray[num64].allocate(2);
is $b.elems, 2, 'a num64 CArray';
is $b[1], 0, '... zeroed';

is CArray[uint8].allocate(2.9).elems, 2, 'a non-Int count is its .Int';
is CArray[Pointer].allocate(2).elems, 2, 'a Pointer CArray';
