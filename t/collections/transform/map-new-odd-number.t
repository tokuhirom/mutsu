use Test;

plan 11;

sub died-with(&c) { my $r; try { c(); CATCH { default { $r = .^name ~ ' ' ~ .message.lines.join('|') } } }; $r }

is died-with({ Map.new(1) }), 'X::Hash::Store::OddNumber Odd number of elements found where hash initializer expected:|Only saw: 1', 'Map.new with one element';
is died-with({ Hash.new("a") }), 'X::Hash::Store::OddNumber Odd number of elements found where hash initializer expected:|Only saw: "a"', 'Hash.new with one string';
is died-with({ Map.new(1, 2, 3) }), 'X::Hash::Store::OddNumber Odd number of elements found where hash initializer expected:|Found 3 (implicit) elements:|Last element seen: 3', 'three elements';
is died-with({ Map.new((1, 2, 3)) }), 'X::Hash::Store::OddNumber Odd number of elements found where hash initializer expected:|Found 3 (implicit) elements:|Last element seen: 3', 'a list of three';
is died-with({ Hash.new(1, 2, :a(3), 4) }), 'X::Hash::Store::OddNumber Odd number of elements found where hash initializer expected:|Found 3 (implicit) elements:|Last element seen: 4', 'a Pair is a complete entry';

is Map.new("a", 1, "b", 2).elems, 2, 'an even list is fine';
is Map.new((a => 1), (b => 2)).elems, 2, 'pairs are fine';
is Hash.new(:a(1), :b(2)).elems, 2, 'all-named is data';
is Map.new("a", 1, :b(2)).raku, 'Map.new((:a(1)))', 'named dropped beside positionals';
is Map.new.elems, 0, 'empty';
is died-with({ Map.new("k", Any) }), Any, 'a type-object value is a value';
