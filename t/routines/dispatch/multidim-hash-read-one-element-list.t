use Test;

plan 5;

my %hash = a => { b => { c => 42 } };
is-deeply %hash{"a";"b";"c"}, (42,),
    'an associative multi-dimensional scalar-key read is a one-element List';
is %hash{"a";"b";"c"}.raku, '(42,)',
    'method dispatch observes the List wrapper';
is-deeply %hash{"a";"b";"missing"}, (Any,),
    'a missing nested hash key keeps the List wrapper';
sub replace-slice(\slice) { slice = (99,) }
replace-slice(%hash{"a";"b";"c"});
is %hash<a><b><c>, 99, 'a raw parameter still writes through the selected leaf';

my @array = [[1, 2], [3, 4]];
is @array[1;0], 3, 'a positional multi-dimensional read remains a scalar';
