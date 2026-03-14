use Test;

plan 7;

{
    my $*x = 1;
    ok $*x.VAR.dynamic, '$* scalar reports dynamic';
}

{
    my $x = 1;
    nok $x.VAR.dynamic, '$ scalar reports non-dynamic';
}

{
    my @*x = 1, 2, 3;
    ok @*x.VAR.dynamic, '@* array reports dynamic';
    ok @*x[0].VAR.dynamic, '@* array element reports dynamic';
}

{
    my %*x = a => 1;
    ok %*x.VAR.dynamic, '%* hash reports dynamic';
    ok %*x<a>.VAR.dynamic, '%* hash element reports dynamic';
}

{
    my $x is dynamic = 1;
    ok $x.VAR.dynamic, 'is dynamic trait still reports dynamic';
}
