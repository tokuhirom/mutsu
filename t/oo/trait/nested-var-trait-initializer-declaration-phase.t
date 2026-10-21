use Test;

# A user `trait_mod:<is>(Variable ...)` on a variable with an initializer in a
# nested scope runs in the declaration phase (before the initializer's
# run-time evaluation), once per declaration, not once per execution.

plan 5;

my @log;
my $shade = 7;
multi sub trait_mod:<is>(Variable $v, :$tag!) { @log.push($tag.raku) }

BEGIN { @log.push('begin') }
sub twice($n) { my $b is tag($shade) = $n * 2; $b }
{ my $c is tag(1) = 5; }

is @log.join(','), 'begin,Any,1,2', 'handlers ran at declaration time, argument unevaluated';
is twice(4), 8, 'the initializer still runs on each call';
is twice(5), 10, 'second call';
is @log.elems, 4, 'calling the sub did not apply the trait again';
{
    my $x is tag(2) = 3;
    $x++;
    is $x, 4, 'the variable keeps its initializer value';
}
