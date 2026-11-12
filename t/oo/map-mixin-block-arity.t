use Test;

# A role mixed into a multi-parameter Block keeps the Block's arity for
# `.map` / `.grep`: the callable takes its elements a group at a time (found
# running ParaSeq, whose `BlockMapper` role is mixed into the user's block;
# `map(-> $a, $b { $a })` at degree 3 returned nothing).

plan 5;

my role Marker { method marked { True } }
my &pair = -> $a, $b { $a + $b };

is-deeply (1..6).map(&pair but Marker).List, (3, 7, 11), 'map takes two elements per call';
is-deeply (1..6).grep(-> $a, $b { $a > 2 } but Marker).List, ((3, 4), (5, 6)), 'grep takes two elements per call';
is-deeply (1..6).map(&pair but Marker).Array.List, (3, 7, 11), 'and so does an eager map';
throws-like { my @x = (1..5).map(&pair but Marker) }, Exception,
    message => /'Too few positionals'/, 'a short final group dies, as on rakudo';
is-deeply (1..3).map({ $_ * 2 } but Marker).List, (2, 4, 6), 'a one-parameter block is unchanged';
