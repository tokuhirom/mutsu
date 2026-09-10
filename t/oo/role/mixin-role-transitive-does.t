use v6;
use Test;

# A role mixed into a VALUE (`but` / runtime `does`) may itself compose other
# roles (`role R1 does R2 {}`); the mixed value must then match R2 too.
# Class composition (`class C does R1`) already handled this — only the mixin
# path lost the transitive roles. This is the JSON::Name t/020-trait.t shape:
#   role NamedAttribute does JSON::OptIn::OptedInAttribute { ... }
#   $attr does NamedAttribute;   # must then .does(OptedInAttribute)

plan 8;

role R3 { }
role R2 does R3 { method r2m { "r2m" } }
role R1 does R2 { }

my $o = 42 but R1;
ok $o.does(R1), 'mixin does the directly mixed role';
ok $o.does(R2), 'mixin does the role composed by the mixed role';
ok $o.does(R3), 'transitive composition is followed to any depth';
ok $o ~~ R2, 'smartmatch follows the composed role too';
is $o.r2m, 'r2m', 'a method from the composed role dispatches';

class C { has $.a }
my $attr = C.^attributes[0];
$attr does R1;
ok $attr.does(R2), 'runtime does on an Instance follows composed roles';

role P2[::T] { }
role P1 does P2[Int] { }
my $p = 1 but P1;
ok $p.does(P2), 'a parametric composed role matches by base name';

role Other { }
nok $o.does(Other), 'an unrelated role still does not match';
