use v6;
use Test;

# `HyperSeq`/`RaceSeq` expose `.configuration`, whose `.batch`/`.degree` report
# the values the sequence was hyperized with. Regression pin for the `hyperize`
# dist (whose subs default `:batch`/`:degree` off `Iterable.hyper.configuration`).

my @a = ^10;

# Explicit batch/degree round-trip through .configuration.
{
    my $c := @a.hyper(:batch(12), :degree(16)).configuration;
    is $c.batch,  12, 'hyper .configuration.batch reports the requested batch';
    is $c.degree, 16, 'hyper .configuration.degree reports the requested degree';
}

# Only batch supplied — degree falls back to a positive default.
{
    my $c := @a.hyper(:batch(42)).configuration;
    is $c.batch, 42, 'hyper .configuration.batch with only :batch supplied';
    ok $c.degree > 0, 'hyper .configuration.degree defaults to a positive value';
}

# .race carries its configuration the same way.
{
    my $c := @a.race(:batch(7), :degree(3)).configuration;
    is $c.batch,  7, 'race .configuration.batch reports the requested batch';
    is $c.degree, 3, 'race .configuration.degree reports the requested degree';
}

# The default configuration off the Iterable type object has positive values,
# so the `hyperize` INIT block (`Iterable.hyper.configuration`) can read them.
{
    my $c := Iterable.hyper.configuration;
    ok $c.batch  > 0, 'Iterable.hyper.configuration.batch has a positive default';
    ok $c.degree > 0, 'Iterable.hyper.configuration.degree has a positive default';
}

done-testing;
