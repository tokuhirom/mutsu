use Test;

# ADR-11276 slice 3: List, Array and Map invert use shared handler rows.
plan 6;

my @pairs = (a => 1, b => (2, 3));
is-deeply @pairs.invert.map({ .key.Str ~ "=" ~ .value.Str }).List, <1=a 2=b 3=b>,
    'Array.invert expands Pair values';

my $list = @pairs.List;
is-deeply $list.invert.map({ .key.Str ~ "=" ~ .value.Str }).List, <1=a 2=b 3=b>,
    'List.invert uses the same Pair expansion';

my $map = Map.new(a => 1, b => (2, 3));
is-deeply $map.invert.map({ .key.Str ~ "=" ~ .value.Str }).sort.List, <1=a 2=b 3=b>,
    'Map.invert expands values';

my %hash = a => 1, b => 2;
is-deeply %hash.invert.map({ .key.Str ~ "=" ~ .value.Str }).sort.List, <1=a 2=b>,
    'Hash.invert inherits Map row';

my %object{Any} = 1 => 'one';
is %object.invert.head.key.^name, 'Str',
    'Hash.invert keeps the declared key representation';

ok List.^can('invert') && Array.^can('invert') && Map.^can('invert'),
    'the shaped collection owners expose invert rows';
