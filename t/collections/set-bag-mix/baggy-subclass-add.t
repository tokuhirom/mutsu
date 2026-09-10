use Test;

plan 6;

# `(+)` of two instances of the SAME Bag subclass returns that subclass,
# not a plain Bag (roast S02-types/bag.t #252, rakudo#5190).
my class Foo is Bag {}

my $u = Foo.new("a") (+) Foo.new("b");
isa-ok $u, Foo, 'Foo (+) Foo is a Foo';
isa-ok $u, Bag, 'the result is still a Bag';
is $u<a>, 1, 'union kept the "a" weight';
is $u<b>, 1, 'union kept the "b" weight';

# Two plain Bags still union to a plain Bag (no spurious subclass wrapping).
my $p = bag(<a b>) (+) bag(<b c>);
isa-ok $p, Bag, 'Bag (+) Bag is a plain Bag';
is $p<b>, 2, 'plain-Bag union adds weights';
