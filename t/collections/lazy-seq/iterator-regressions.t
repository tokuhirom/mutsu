use Test;

plan 5;

sub run-twice(&iterator) {
    my $iterator = iterator();
    my $iterator = iterator();
    $iterator
}

is run-twice({ 42 }), 42, 'callable parameter is not shadowed by scalar of same bare name';

my $iter = (1..5).iterator;
Nil until $iter.push-exactly(my @a, 2) =:= IterationEnd;
is-deeply @a, [1, 2, 3, 4, 5], 'my @a in expression position is initialized once per declaration';

{
    my @a;
    @a.push(99);
}
{
    my $iter = (1..3).iterator;
    Nil until $iter.push-exactly(my @a, 2) =:= IterationEnd;
    is-deeply @a, [1, 2, 3], 'expression-position my @a does not leak prior block content';
}

my @pairs = (1..3).pairs;
my $pair-iter = @pairs.iterator;
ok $pair-iter.skip-at-least(@pairs - 1), 'pairs iterator skip-at-least works';
is $pair-iter.pull-one, @pairs[* - 1], 'is() accepts Pair as a positional expected value';
