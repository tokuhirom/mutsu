use v6.e.PREVIEW;
use Test;

# From the RedFactory distribution: `method ^model($f) is rw { $ }` is an
# anonymous state container, and each type that inherits the metamethod gets
# its own (rakudo gives every type's HOW a separate copy).

plan 5;

class Base {
    has %!d;
    method ^model($f) is rw { $ }
    method ^data($f) is rw { $f.^attributes.first(*.name eq '%!d').get_value: $f }
}
class C is Base { }
class D is Base { }
C.^model = 3;
D.^model = 4;
is C.^model, 3, 'first type keeps its own state';
is D.^model, 4, 'second type has separate state';

my \A = Metamodel::ClassHOW.new.new_type: :name<A>;
A.^add_parent: Base; A.^compose;
my \B = Metamodel::ClassHOW.new.new_type: :name<B>;
B.^add_parent: Base; B.^compose;
A.^model = 1;
B.^model = 2;
is-deeply (A.^model, B.^model), (1, 2), 'dynamically built types too';

# `$obj.^meta{key} = v` assigns through the rw metamethod, not a method `meta`.
my $c = C.new;
$c.^data{"x"} = 1;
is-deeply $c.^data, {:x(1)}, 'subscript assignment on a ^metamethod call';
$c.^data<y> = 2;
is-deeply $c.^data, {:x(1), :y(2)}, 'angle-bracket subscript too';
