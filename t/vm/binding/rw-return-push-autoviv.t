use v6;
use Test;

# Found via the Math::Symbolic distribution: its MultiHash.elem is
# `method elem(...) is rw { %!hash{ $%key } }` and callers do
# `$multihash.elem(...).push: ...` on a key that does not exist yet.

plan 8;

my %h;
sub f($k) is rw { %h{$k} }
f('x').push: 5;
is-deeply %h<x>, [5], 'push on an rw routine returning a missing hash entry vivifies an Array';
f('x').push: 6;
is-deeply %h<x>, [5, 6], 'a second push appends to the vivified Array';
f('y').append: 1, 2;
is-deeply %h<y>, [1, 2], 'append vivifies too';

my %g;
sub g($k) is rw { %g{$k} }
my $e := g('z');
$e.push: 5;
$e.append: 6, 7;
is-deeply %g<z>, [5, 6, 7], 'a variable bound to the missing entry pushes through it, repeatedly';

class A { has %.h; method e($k) is rw { %!h{$k} } }
my $a = A.new;
$a.e('x').push: 5;
is-deeply $a.h<x>, [5], 'push on an rw method returning a missing attribute hash entry';

class B { has %.h{Any}; method e(*%kh is copy) is rw { my %key := %kh; %!h{ $%key } } }
my $b = B.new;
$b.e(:t(1)).push: 5;
$b.e(:t(2)).push: 6;
is $b.h.elems, 2, 'object-hash attribute: each distinct key vivifies its own entry';
my $elem := $b.e(:t(3));
$elem.push: 7;
$elem.append: 8, 9;
is $b.h.elems, 3, 'bound entry of an object-hash attribute is stored';
is-deeply $b.h.values.sort(*.elems).tail, [7, 8, 9], 'and holds every pushed value';
