use Test;

plan 6;

# The type object of `Class.^mixin(R)` finds the accessors of R's attributes,
# so an alias can be added with `^add_method`.
role R { has $.b is rw = 2; method hi { 1 } }
class Baz { has $.c; }
my $t = Baz.^mixin(R);

ok $t.^find_method('hi').defined, "a role's method is found";
ok $t.^find_method('b').defined, "a role attribute's accessor is found";
ok $t.^find_method('c').defined, "the base class's accessor is found";

$t.^add_method('nk', $t.^find_method('b'));
my $y = $t.new;
is $y.nk, 2, 'an alias of the role attribute accessor reads it';
$y.nk = 8;
is $y.b, 8, 'and writes it';
ok $t.^can('nk').elems >= 1, 'the alias is visible to .^can';
