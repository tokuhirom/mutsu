use Test;

# Binding to a native-int parameter binds the coerced value on every binder
# (sub, light-call sub, method, pointy block): the argument wraps to the
# declared width, an Int-valued enum (and Bool) unboxes to its integer, and an
# itemized argument (`my $ = 200`) binds what it holds (issue #9533).
#
# Rakudo wraps on bind too; only a *literal constant* argument returned
# unchanged comes back unwrapped (its optimizer inlines the literal), so every
# case here passes the argument through a variable or uses the parameter.

plan 19;

enum E <Zero One Two>;

sub u32(uint32 $n) { $n }
sub u8(uint8 $n)   { $n }
sub i8(int8 $n)    { $n }
sub i(int $n)      { $n }

my $big = 2**33 + 1;
my $neg = -1;
my $v300 = 300;
is u32($big), 1, 'uint32 wraps 2**33+1';
is u32($neg), 4294967295, 'uint32 wraps -1';
is u8($v300), 44, 'uint8 wraps 300';

is u8(Two).raku, '2', 'an Int-valued enum unboxes for a sized native param';
is i(Two).raku, '2', 'an Int-valued enum unboxes for int';
is i(True).raku, '1', 'Bool unboxes for int';
nok (try i(E)).defined, 'a type object cannot bind to int';

is i8(my $ = 200), -56, 'an itemized argument binds its value (int8)';
is i(my $ = 5), 5, 'an itemized argument binds its value (int)';

class C {
    method m(int8 $n) { $n }
    method k(int $n)  { $n }
}
is C.m(my $ = 200), -56, 'method: native param wraps';
is C.m(Two), 2, 'method: enum unboxes';
is C.k(True).raku, '1', 'method: Bool unboxes';
nok (try C.k(2**70)).defined, 'method: out-of-range int dies';

is (-> int16 $n { $n })(my $ = 70000), 4464, 'pointy block: int16 wraps';

my int $x = Two;
is $x.raku, '2', 'my int $x = <enum> stores the value';
my int8 $y = Two;
is $y, 2, 'my int8 $y = <enum> stores the value';
my uint8 @a = Two, One;
is @a.raku, 'array[uint8].new(2, 1)', 'native array stores enum values';

# An `is rw` native parameter still binds the caller's container.
sub bump(int $p is rw) { $p = $p + 1 }
my int $q = 1;
bump($q);
is $q, 2, 'int $p is rw writes back to the caller';
class D { method bump(int $p is rw) { $p = $p + 1 } }
my int $r = 5;
D.bump($r);
is $r, 6, 'method: int $p is rw writes back to the caller';
