use v6;
use Test;

plan 4;

# A role called as a type object (`R.method`) is punned to a class of the
# same name. A later `also does R` in another role must still compose R's
# attributes into a `but` mixin (#14076).
use lib 't/lib';
use PunnedRoleBase14076;

my $r = PunnedRoleBase14076.required('PunnedRoleDoer14076');
my $c = True but $r;
is $c.can('obj-num').elems, 1, 'composed attribute reachable after require inside the role method';
$c.obj-num = 6;
is $c.obj-num, 6, 'composed attribute is writable';
is $c.content, 1, 'own method works';

role A14076 { has Int $.n is rw; method hi { 1 } }
A14076.hi;
role B14076 { also does A14076 }
is (True but B14076).can('n').elems, 1, 'also does a punned role composes its attributes';
