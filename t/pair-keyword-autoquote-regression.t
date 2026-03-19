use Test;

plan 4;

{
    my %a = (a => 1, b => 'foo', c => Mu);
    ok !(%a ~~ b => 'ugh'), 'smartmatch keeps pair value on RHS';
    ok !(%a ~~ a => 'foo'), 'smartmatch does not bind => outside the comparison';
}

{
    my @items;
    my $result = push @items, enc => 'utf-8';
    is @items[0].raku, ':enc("utf-8")', 'listop call keeps fat-arrow arg as a pair';
    is $result.raku, '[:enc("utf-8")]', 'listop result is not wrapped in an outer pair';
}
