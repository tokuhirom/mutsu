use Test;

# A nested group in a declaration (`my ($a, ($b, $c)) = ...`) destructures
# the RHS element at its position (#15452; found by the ADR-15334 slice 4
# three-path parity test).
plan 7;

{
    my ($a, ($b, $c)) = 1, (2, 3);
    is $a ~ $b ~ $c, '123', 'nested group destructures its element';
}
{
    my ($d, ($e, @f), $g) = 1, (2, 3, 4), 5;
    is "$d $e {@f.raku} $g", '1 2 [3, 4] 5',
        'a slurpy inside the group takes only the group rest; later targets keep their position';
}
{
    my ($h, ($i, ($j, $k)), $l) = 1, (2, (3, 4)), 5;
    is "$h$i$j$k$l", '12345', 'groups nest to any depth';
}
{
    my (\m, (\n, \o)) = 1, (2, 3);
    is m ~ n ~ o, '123', 'sigilless targets in a nested group';
}
{
    my ($p, ($q, $r)) := 1, (2, 3);
    is $p ~ $q ~ $r, '123', 'nested group in a binding declaration';
}
{
    my ($x, ($y, $z)) = 1, (2,);
    is $x ~ $y, '12', 'a short nested element leaves the rest unset';
    ok !$z.defined, 'the unreached target is undefined';
}
