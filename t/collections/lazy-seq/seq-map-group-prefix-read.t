use Test;

# A prefix read of a deferred `.map` with a multi-parameter block runs the
# callback only as far as the read needs, one whole group per call, as
# Rakudo's iterator does; it used to run over the whole source (#14196).
# Expected values are the `raku` (v2026.09) output.

plan 9;

my $n = 0;
my $s = (1..*).map(-> Int $x { $n++; $x });
is $s[2], 3, 'a typed-parameter map over an infinite source';
is $n, 3, '... runs the callback three times';

my $k = 0;
my $u = (1..10).map(-> $x, $y { $k++; $x + $y });
is $u[1], 7, 'a two-parameter map answers the second group';
is $k, 2, '... and runs the callback twice';

my $c = 0;
my $chained = (1..10).map({ $_ }).map(-> $x, $y { $c++; $x + $y });
is $chained[1], 7, 'a two-parameter map chained onto another map';
is $c, 2, '... runs its callback twice';

is (1..10).grep(* > 0).map(-> $a, $b { $a * $b }).head(2).join(','), '2,12',
    'a chained two-parameter map pulls whole groups';
is (1..5).map(-> $a, $b { $a + $b }).head(1).join(','), '3',
    'an odd-length source answers a prefix without reaching the short group';
is (1..6).map(-> $a, $b, $c { $a + $b + $c }).List.join(','), '6,15',
    'a full read still covers every group';
