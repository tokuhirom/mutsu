use Test;

# A `my` declaration is in its block's pad from the start of its own statement,
# so a `MY::` read in its initializer lists it (#15607). Checked against rakudo.
plan 4;

{
    my $x = 1;
    my @n = MY::.keys.grep(/<[a..z]>/).sort;
    is-deeply @n, ['$x', '@n'], 'an array declared by the statement is listed';
}
{
    my $x = 1;
    my $s = MY::.keys.grep(/<[a..z]>/).sort.join(',');
    is $s, '$s,$x', 'a scalar declared by the statement is listed';
}
{
    my %h = MY::.keys.grep(/<[a..z]>/).sort Z=> 1..9;
    is-deeply %h.keys.sort.list, ('%h',), 'a hash declared by the statement is listed';
}
sub f { my @a = MY::.keys.grep(/<[a..z]>/).sort; @a }
is-deeply f(), ['@a'], 'in a routine body too';
