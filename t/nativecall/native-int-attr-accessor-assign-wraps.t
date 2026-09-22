use Test;

# #9022: assigning an out-of-range value to a narrow (or full-width unsigned)
# native-int scalar attribute *through its accessor* never wrapped. The store
# went through `assign_method_lvalue_with_values`, whose single pre-store
# chokepoint (`check_attr_store_type`) only type-checked the incoming value --
# and `type_matches_value` accepts an in-range-as-Int value like `260` for a
# `uint8` constraint, so nothing ever narrowed it. The ordinary local-slot
# store (`my uint8 $x = 260`) wrapped because it runs
# `wrap_native_int_by_constraint` after the same type check; the attribute
# store now runs it too, so the two spellings of one accessor agree.

plan 12;

class Uns {
    has uint8 $.v is rw;
    method set { $.v = 260 }
}
my $u = Uns.new;
$u.set;
is $u.v, 4, 'uint8 accessor plain assignment from inside a method wraps';

class Uns2 {
    has uint8 $.v is rw = 250;
    method bump { $.v += 10 }
}
my $u2 = Uns2.new;
$u2.bump;
is $u2.v, 4, 'uint8 accessor compound assignment from inside a method wraps';

class Uns3 {
    has uint8 $.v is rw;
}
my $u3 = Uns3.new;
$u3.v = 260;
is $u3.v, 4, 'uint8 accessor plain assignment from outside wraps';

class RwMethod {
    has uint8 $!v;
    method v is rw { $!v }
}
my $rw = RwMethod.new;
$rw.v = 260;
is $rw.v, 4, 'an `is rw` method exposing a uint8 attribute wraps its store';

class Sig {
    has int8 $.v is rw;
}
my $s = Sig.new;
$s.v = 200;
is $s.v, -56, 'int8 accessor assignment wraps into the signed range';

class Neg {
    has uint8 $.v is rw;
}
my $neg = Neg.new;
$neg.v = -1;
is $neg.v, 255, 'a negative value stored through a uint8 accessor wraps like C unsigned';

class Wide {
    has uint $.v is rw;
}
my $w = Wide.new;
$w.v = -1;
is $w.v, 18446744073709551615, 'a negative value stored through a full-width uint accessor wraps';

class N32 {
    has num32 $.f is rw;
}
my $n = N32.new;
$n.f = 1.1e0;
ok $n.f != 1.1e0, 'num32 accessor assignment truncates to single precision';
is-approx $n.f, 1.1e0, 1e-6, 'the truncated num32 is still the same number to float32 precision';

# In-range and untyped attributes must be untouched.
class InRange {
    has uint8 $.v is rw;
}
my $ir = InRange.new;
$ir.v = 200;
is $ir.v, 200, 'an in-range uint8 accessor store is the identity';

class Untyped {
    has $.v is rw;
}
my $un = Untyped.new;
$un.v = 260;
is $un.v, 260, 'an untyped attribute accessor store does not wrap';

class Boxed {
    has Int $.v is rw;
}
my $b = Boxed.new;
$b.v = 260;
is $b.v, 260, 'a boxed Int attribute accessor store does not wrap';
