use Test;

# Found via the Cookie::Jar ecosystem distribution: `do given X -> $p { ... }`
# must bind the pointy parameter to the topic, like the statement form.
plan 6;

is (do given 3 -> $j { $j }), 3, 'do given pointy yields the topic';
my $x = do given 4 -> $j { $j * 2 };
is $x, 8, 'assigned do given pointy';

class J { method m { my %d; %d } }
my $h = do given J.new -> $j { $j.m };
is-deeply $h, {}, 'method call on the pointy param returns an empty Hash';

$_ = 9;
is (do given 3 -> $j { "$_ $j" }), '9 3', 'outer $_ stays visible in a pointy do given';

my @a = 1, 2;
is (do given @a -> @p { @p.elems }), 2, 'array pointy param';
is (do given 5 { $_ + 1 }), 6, 'non-pointy do given unchanged';
