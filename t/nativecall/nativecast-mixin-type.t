use Test;
use nqp;

# `nqp::nativecallcast` to a mixin type object -- what upstream NativeCall's
# `Pointer[T]` and `CArray[T]` are (`^parameterize` returns
# `Base.^mixin(Role[T])`) -- answers an object of exactly that type over the
# C address. A CArray one is an unmanaged view: its elements are the C memory,
# read and written in place. `nqp::box_i` into such a type boxes the same way.
# Self-contained classes in upstream's shape, so the test does not depend on
# which NativeCall implementation `use NativeCall` loads.

plan 12;

class P is repr('CPointer') {
    method Int(P:D:) { nqp::p6box_i(nqp::unbox_i(self)) }
    my role Typed[::T] { method of() { T } }
    method ^parameterize(Mu:U \p, Mu:U \t) {
        my $w := p.^mixin(Typed[t]);
        $w.^set_name("P[{t.^name}]");
        $w
    }
}

class A is repr('CArray') is array_type(P) {
    my role IntTyped[::T] is array_type(T) {
        method AT-POS(Int:D $i) is raw { nqp::atposref_i(self, $i) }
        method ASSIGN-POS(Int:D $i, \v) { nqp::bindpos_i(self, $i, v) }
    }
    method ^parameterize(Mu:U \a, Mu:U \t) {
        my $w := a.^mixin(IntTyped[t]);
        $w.^set_name("A[{t.^name}]");
        $w
    }
}

my $owner := nqp::create(A[int32]);
$owner.ASSIGN-POS($_, ($_ + 1) * 10) for ^3;
my $addr = nqp::nativecallcast(P, P, $owner).Int;
ok $addr > 0, 'a CArray casts to a pointer at its storage';

my $p := nqp::nativecallcast(P[int32], P[int32], $owner);
is $p.^name, 'P[int32]', 'a cast to a mixin pointer type has that type';
is $p.of.^name, 'int32', 'and keeps the role methods';
is $p.Int, $addr, 'and holds the address';

my $v := nqp::nativecallcast(A[int32], A[int32], $p);
is $v.^name, 'A[int32]', 'a cast to a mixin CArray type has that type';
is $v.AT-POS(1), 20, 'its elements read the C memory';
$v.AT-POS(2) = 99;
is $owner.AT-POS(2), 99, 'a write through an element reference lands in C memory';
$v.ASSIGN-POS(0, 7);
is $owner.AT-POS(0), 7, 'nqp::bindpos_i writes the C memory too';
dies-ok { nqp::elems($v) }, 'its elems is unknown, as for any C array from a library';

ok nqp::nativecallcast(A[int32], A[int32], P) =:= A[int32],
    'a NULL source casts to the type object';

my $b := nqp::box_i($addr, P[int32]);
is $b.^name, 'P[int32]', 'nqp::box_i into a mixin pointer type boxes as that type';
is $b.Int, $addr, 'and holds the address';
