use Test;

plan 6;

# annotations (ecosystem) t/01-containers: `is %@keeper, ...`.
my @pairs = 'a', 1, 'b', 2;
is-deeply %@pairs, {a => 1, b => 2}, '%@a is a.hash';
my $h = %@pairs;
is-deeply $h, {a => 1, b => 2}, '%@a in an initializer';
is-deeply (%@pairs), {a => 1, b => 2}, '%@a inside parens';
is %@pairs.elems, 2, '%@a takes a method call';

my %hash = x => 1;
is-deeply @%hash, (x => 1,), '@%h is h.list';
is-deeply [@%hash.map(*.key)], ['x'], '@%h takes a method call';
