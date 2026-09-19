use Test;

plan 4;

my @units = (<hour hours> => 60, <minute minutes> => 60, <second>);
for @units {
    $_ ~~ Pair or $_ = $_ => 1;
    $_ ~~ Pair and .key ~~ Str and $_ = (.key, .key ~ 's') => .value;
}

is @units[2].^name, 'Pair', 'a fat arrow inside loose logical operands constructs a Pair';
is @units[2].key[0], 'second', 'the reassociated Pair keeps its singular key';
is @units[2].key[1], 'seconds', 'the reassociated Pair keeps its plural key';
is @units[2].value, 1, 'the assignment receives the Pair value';
