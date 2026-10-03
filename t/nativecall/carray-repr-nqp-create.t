use Test;
use nqp;

# `is repr('CArray')` selects the REPR by declaration, not by class name
# (#11209, ADR-11203). This mirrors how upstream `NativeCall::Types` builds a
# typed array: a role whose `is array_type(TValue)` names the element type is
# mixed in by `^parameterize`, and every element access is nqp code on `self`.

plan 22;

class Ptr is repr('CPointer') { }
class CA is repr('CArray') is array_type(Ptr) {
    my role IntTyped[::TValue] is array_type(TValue) {
        method AT-POS(::?CLASS:D: $pos) is raw {
            nqp::atposref_i(self, nqp::unbox_i($pos.Int))
        }
        method ASSIGN-POS(::?CLASS:D: $pos, $v) {
            nqp::bindpos_i(self, nqp::unbox_i($pos.Int), nqp::unbox_i($v.Int))
        }
    }
    my role UIntTyped[::TValue] is array_type(TValue) {
        method AT-POS(::?CLASS:D: $pos) is raw {
            nqp::atposref_u(self, nqp::unbox_i($pos.Int))
        }
        method ASSIGN-POS(::?CLASS:D: $pos, $v) {
            nqp::bindpos_u(self, nqp::unbox_i($pos.Int), nqp::unbox_u($v.Int))
        }
    }
    my role NumTyped[::TValue] is array_type(TValue) {
        method AT-POS(::?CLASS:D: $pos) is raw {
            nqp::atposref_n(self, nqp::unbox_i($pos.Int))
        }
        method ASSIGN-POS(::?CLASS:D: $pos, $v) {
            nqp::bindpos_n(self, nqp::unbox_i($pos.Int), $v.Num)
        }
    }
    method ^parameterize(Mu:U \array, Mu:U \t) {
        my $role := nqp::istype(t, Num)
          ?? NumTyped[t.WHAT]
          !! t.^unsigned ?? UIntTyped[t.WHAT] !! IntTyped[t.WHAT];
        my $what := array.^mixin($role);
        $what.^set_name(array.^name ~ '[' ~ t.^name ~ ']');
        $what
    }
    method elems(::?CLASS:D:) { nqp::elems(self) }
}

is CA.REPR, 'CArray', 'the type object reports its declared REPR';
is CA.^array_type.^name, 'Ptr', 'the class records its own array_type';

my \T = CA.^parameterize(int32);
is T.^name, 'CA[int32]', 'the parameterized type is named';
is T.^array_type.^name, 'int32', 'a mixed-in role array_type answers for the mixin type';

my $a := nqp::create(T);
is $a.^name, 'CA[int32]', 'nqp::create keeps the mixin type';
is $a.REPR, 'CArray', 'the instance has CArray storage';
is nqp::elems($a), 0, 'it starts empty';
nqp::bindpos_i($a, 2, 5);
is nqp::elems($a), 3, 'a bind past the end grows it';
is nqp::atpos_i($a, 2), 5, 'the element reads back';
is nqp::atpos_i($a, 0), 0, 'the gap is zero-filled';
$a.ASSIGN-POS(0, 2**32 + 7);
is $a.AT-POS(0), 7, 'an int32 element wraps at its width';
$a.ASSIGN-POS(1, -1);
is $a.AT-POS(1), -1, 'an int32 element is signed';
is $a.elems, 3, 'elems through a class method';

my $r := $a.AT-POS(1);
$r = 42;
is $a.AT-POS(1), 42, 'a write through the element reference lands in the array';
is $r, 42, 'the reference reads the element back';
nqp::bindpos_i($a, 1, 9);
is $r, 9, 'the reference sees a later write to the array';

my \U = CA.^parameterize(uint8);
my $u := nqp::create(U);
$u.ASSIGN-POS(0, 300);
is $u.AT-POS(0), 44, 'a uint8 element wraps unsigned';
$u.ASSIGN-POS(1, -1);
is $u.AT-POS(1), 255, 'and has no sign';

my \N = CA.^parameterize(num32);
my $n := nqp::create(N);
$n.ASSIGN-POS(0, 1.5e0);
is $n.AT-POS(0), 1.5e0, 'a num32 element round-trips';
is nqp::elems($n), 1, 'and counts one element';

# A REPR is not inherited.
class SubCA is CA { }
is SubCA.REPR, 'P6opaque', 'a subclass of a CArray class is not a CArray';
is nqp::create(SubCA).REPR, 'P6opaque', 'nor is its instance';
