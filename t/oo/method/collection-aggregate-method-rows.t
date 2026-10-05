use Test;

plan 20;

my @array = 1, 3, 2;
my $list = (1, 3, 2).List;

is @array.sum, 6, 'Array.sum uses its method row';
is $list.sum, 6, 'List.sum uses its method row';
is @array.minmax, 1..3, 'Array.minmax returns the inclusive bounds';
is $list.minmax, 1..3, 'List.minmax returns the inclusive bounds';
is @array.permutations.elems, 6, 'Array.permutations remains lazy and complete';
is-deeply @array.permutations[1], (1, 2, 3), 'Array.permutations preserves iteration order';
is @array.combinations.elems, 8, 'Array.combinations includes every subset';
is-deeply @array.combinations, ((), (1,), (3,), (2,), (1, 3), (1, 2), (3, 2), (1, 3, 2)),
    'Array.combinations preserves iteration order';
is (1, "0xff").sum, 256, 'List.sum keeps Raku numeric string parsing';
dies-ok { (1, "not numeric").sum }, 'List.sum keeps numeric conversion failures';
is (1..4).sum, 10, 'Range.sum remains on the cascade path';
is-deeply (3, 1, 2).Seq.minmax, 1..3, 'Seq.minmax remains on the cascade path';
is 7.sum, 7, 'Int.sum uses its Any row';
is "42".sum, 42, 'Str.sum parses its numeric value';
is True.sum, 1, 'Bool.sum numerically coerces the receiver';
is (1 + 2i).sum, 1 + 2i, 'Complex.sum preserves the numeric value';
is (1/3).FatRat.sum, 1/3, 'FatRat.sum preserves exactness';
is 5.minmax, 5..5, 'scalar minmax returns a single-value range';
is %(a => 1).minmax.raku, ':a(1)..:a(1)', 'Hash.minmax uses its Pair list';
throws-like { %(a => 1).sum }, X::Multi::NoMatch,
    'Hash.sum reports that its Pairs are not Numeric';
