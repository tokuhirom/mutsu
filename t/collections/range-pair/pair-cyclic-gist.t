use Test;

plan 6;

# `1 => $b` keeps `$b`'s container as the Pair's value, so assigning it back
# closes a cycle. Rakudo names the node the walk loops back to.
my $b = 1;
for ^3 { $b = 1 => $b }

my $g = $b.gist;
like $g, /^ '(\\Pair_' (\d+) ' = 1 => Pair_' $0 ')' $/, 'gist names the cyclic Pair';
my $r = $b.raku;
like $r, /^ '(my \\Pair_' (\d+) ' = 1 => Pair_' $0 ')' $/, 'raku names the cyclic Pair';
like (say-capture $b), /^ '(\\Pair_'/, 'say renders instead of recursing';

sub say-capture($x) { $x.gist }

# A Pair reachable twice without nesting is not a cycle.
my $p = 1 => 2;
is [$p, $p].gist, '[1 => 2 1 => 2]', 'a shared Pair is not a cycle';
is [$p, $p].raku, '[1 => 2, 1 => 2]', 'a shared Pair is not a cycle in raku';

# A cycle through an array element.
my @a;
my $q = 'k' => @a;
@a.push($q);
lives-ok { $q.gist }, 'a Pair/Array cycle renders';

done-testing;
