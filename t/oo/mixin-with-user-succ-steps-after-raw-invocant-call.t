use Test;
use NativeCall;
use nqp;

# A value boxed over a native integer with a user `succ` (a NativeCall
# `Pointer`) keeps its type under `++` after a method with a raw invocant
# parameter (`deref(\ptr:)`) was called on the variable. That call parks the
# variable in a shared cell, and the cell's generic step used to treat the
# mixin as a bare `Int` (NativeHelpers::Blob's `t/03-pointer.t`).

plan 4;

NativeCall::Types::Pointer.^add_method('succ', method (Pointer:D:) {
    nqp::box_i(nqp::unbox_i(nqp::decont(self)) + 2, Pointer[uint16]);
});

my CArray[uint16] $a .= new: 10, 20, 30;
my $p = nativecast(Pointer[uint16], $a);

is $p.deref, 10, 'deref reads the first element';
my $before = $p.Int;
$p++;
is $p.^name, 'NativeCall::Types::Pointer[uint16]', '++ after the raw-invocant call keeps the type';
is $p.Int - $before, 2, '...and used the user succ';
is $p.deref, 20, '...so deref reads the next element';
