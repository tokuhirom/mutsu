use v6;
use Test;

plan 15;

role R { }

my %bound := %(2 => 3) but R;
is %bound.raku, '{"2" => 3}', 'binding a Hash mixin keeps its initial entry';
ok %bound<2>:exists, 'the initial Hash mixin entry exists';
is %bound.elems, 1, 'the bound Hash mixin has one entry before storing';

%bound<3> = 4;
is %bound.raku, '{"2" => 3, "3" => 4}', 'storing through a Hash mixin preserves old entries';
is %bound.keys.sort.join(','), '2,3', 'storing through a Hash mixin keeps both keys';
is %bound.elems, 2, 'storing through a Hash mixin updates elems';

my $deleted = %bound<2>:delete;
is $deleted, 3, 'deleting through a Hash mixin returns the old value';
nok %bound<2>:exists, 'deleting through a Hash mixin removes the entry';
is %bound.raku, '{"3" => 4}', 'deleting through a Hash mixin preserves the other entry';

my %assigned = %(2 => 3) but R;
is %assigned.raku, '{"2" => 3}', 'assignment from a Hash mixin copies its entries';
%assigned<3> = 4;
is %assigned.keys.sort.join(','), '2,3', 'an assigned Hash mixin remains a normal Hash';

my %typed := %(2 => 3) but Associative[Int, Int];
is %typed.of, Int, 'a parameterized Associative mixin reports its value type';
is %typed{'2'}, 3, 'a parameterized Associative mixin keeps its entry';
%typed<3> = 4;
is %typed.elems, 2, 'a parameterized Associative mixin remains writable';
is %typed.raku, '{"2" => 3, "3" => 4}', 'a parameterized Associative mixin keeps both entries';

done-testing;
