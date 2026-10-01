use v6;
use Test;

# A role body's `:=` declarations are deferred to composition time and run one
# statement at a time. The parser wraps such a declaration in a group whose
# markers tell the compiler it binds rather than assigns, so the group must be
# kept whole — as a class body already keeps it (Math::Interval's
# `my \x := $x` idiom). Split apart, a sigilless bind snapshotted the value and
# a later write through it died with "Cannot modify an immutable value".
#
# `my ($p, $q) := ...` nests its bind group inside an outer destructuring group,
# which the body plans look through at any depth.

plan 4;

my $y = 1;

role R {
    my \x := $y;
    my ($p, $q) := (10, 20);
    method pq { $p + $q }
    method bump { x = x + 1 }
}

class C does R {}

lives-ok { C.new.bump }, 'a write through a role-body sigilless bind lives';
is C.new.pq, 30, 'a role-body list bind declares its variables';

class D {
    my ($p, $q) := (10, 20);
    method pq { $p + $q }
}

is D.new.pq, 30, 'a class-body list bind declares its variables';

role S {
    my ($a, $b) := (1, 2);
    method s { $a + $b }
}

is (class :: does S {}).new.s, 3, 'an anonymous class composes the same role body';
