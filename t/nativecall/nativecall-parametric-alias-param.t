use Test;

# A constant naming a parameterized type (`CArray[int32]`) is a type in a
# signature: a routine's parameter typed with it binds the arrays of that
# parameterization and refuses any other. This is how DBIish::Pg spells its
# `OidArray`.
#
# Every expectation was verified against Rakudo.

plan 9;

use NativeCall;

my constant I32Arr = CArray[int32];

sub first-of(I32Arr $a) { $a[0] }
my $arr = CArray[int32].new(7, 8);
is first-of($arr), 7, 'a sub binds the aliased parameterization';
dies-ok { first-of(CArray[int64].new(1)) }, 'and refuses another element type';
dies-ok { first-of(Array.new(1)) }, 'and a non-CArray';

class Holder {
    method sum(I32Arr $a) { $a[0] + $a[1] }
}
is Holder.new.sum($arr), 15, 'a method binds it too';
dies-ok { Holder.new.sum(CArray[num64].new(1e0, 2e0)) }, 'and refuses another element type';

multi pick-one(I32Arr $a) { 'i32' }
multi pick-one(CArray[int64] $a) { 'i64' }
is pick-one($arr), 'i32', 'a multi candidate is told apart by the alias';
is pick-one(CArray[int64].new(1)), 'i64', 'from the spelled-out one';

sub c-qsort(I32Arr $base, size_t $n, size_t $size, &cmp (Pointer, Pointer --> int32))
    is native('c', v6) is symbol('qsort') { * }
lives-ok { NativeCall::check_routine_sanity(&c-qsort) }, 'a native routine accepts the alias as a parameter type';

my I32Arr $typed = CArray[int32].new(1, 2);
is $typed.elems, 2, 'a variable typed with the alias holds the array';
