use Test;

# A pointer-shaped parameter of a native callback is upstream's
# NativeCall::Types::Pointer, as in rakudo: it has .Int, is defined, and
# nativecast(Pointer[T], ...) reads through it.

use NativeCall;

plan 6;

sub qsort(CArray[int32], size_t, size_t, &cmp (Pointer, Pointer --> int32))
    is native('c', v6) {*}

my $seen;
my $arr = CArray[int32].new(3, 1, 2);
qsort($arr, 3, 4, sub ($a, $b) {
    $seen //= $a;
    nativecast(Pointer[int32], $a).deref <=> nativecast(Pointer[int32], $b).deref;
});

is $arr.list, (1, 2, 3), 'the callback sorted the array';
is $seen.^name, 'NativeCall::Types::Pointer', 'callback argument is the upstream Pointer';
ok $seen ~~ Pointer, 'it smartmatches Pointer';
ok $seen.defined, 'it is defined';
ok $seen.Int > 0, '.Int gives the address';
ok $seen.Int %% 4, 'the address is an int32 element address';

# vim: expandtab shiftwidth=4
