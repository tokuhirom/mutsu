use v6;
use Test;

plan 9;

my %source = a => 1, b => 0;
my $held = %source;
my %nested = inner => %source;

is %source.grep({ .value }).map({ .key }).sort.join(','), 'a',
    'grep sees Hash pairs from a % variable';
is $held.grep({ .value }).map({ .key }).sort.join(','), 'a',
    'grep sees Hash pairs from a scalar-held Hash';
is %nested<inner>.grep({ .value }).map({ .key }).sort.join(','), 'a',
    'grep sees Hash pairs from a Hash element';

is %source.first({ .value }).key, 'a',
    'first sees Hash pairs from a % variable';
is $held.first({ .value }).key, 'a',
    'first sees Hash pairs from a scalar-held Hash';
is %nested<inner>.first({ .value }).key, 'a',
    'first sees Hash pairs from a Hash element';

is %source.classify({ .value }).keys.sort.join(','), '0,1',
    'classify sees Hash pairs from a % variable';
is $held.classify({ .value }).keys.sort.join(','), '0,1',
    'classify sees Hash pairs from a scalar-held Hash';
is %nested<inner>.classify({ .value }).keys.sort.join(','), '0,1',
    'classify sees Hash pairs from a Hash element';

done-testing;
