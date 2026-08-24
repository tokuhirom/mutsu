use Test;
plan 3;

{
    my ($a, $b, $c);
    (($a, $b), $c) «=» ((1, 2), 3);
    is "$a,$b,$c", "1,2,3", 'hyper assignment recursively assigns nested tuple targets';
}

{
    my ($a, $b, $c, $d);
    (($a, ($b, $c)), $d) »=« ((4, (5, 6)), 7);
    is "$a,$b,$c,$d", "4,5,6,7", 'mixed hyper arrows preserve recursive assignment';
}

{
    my ($a, $b, $c);
    my $evaluations = 0;
    (($a, $b), $c) «=» do {
        $evaluations++;
        ((8, 9), 10)
    };
    is "$a,$b,$c/$evaluations", "8,9,10/1", 'nested hyper assignment evaluates its RHS once';
}
