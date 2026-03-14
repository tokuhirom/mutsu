use Test;

plan 10;

sub pair-signatures($items) {
    $items.map({ .key.Str ~ "\t" ~ .value.Str }).sort.List
}

my $hash-invert = {a => 1, b => 2}.invert;
is $hash-invert.^name, 'Seq', 'Hash.invert returns a Seq';
is-deeply pair-signatures($hash-invert), ("1\ta", "2\tb"), 'Hash.invert swaps keys and values';

my $expanded = {a => (1, 2), b => 3..4}.invert;
is $expanded.^name, 'Seq', 'Hash.invert with iterable values returns a Seq';
is-deeply pair-signatures($expanded), ("1\ta", "2\ta", "3\tb", "4\tb"),
    'Hash.invert expands iterable values into separate pairs';

my $list-invert = ((a => 1), (b => (2, 3))).invert;
is $list-invert.^name, 'Seq', 'List.invert returns a Seq';
is-deeply pair-signatures($list-invert), ("1\ta", "2\tb", "3\tb"),
    'List.invert swaps pair lists and expands iterable values';

my $set-invert = set(<a b>).invert;
is $set-invert.^name, 'Seq', 'Set.invert returns a Seq';
is-deeply $set-invert.map(*.value.Str).sort.List, <a b>, 'Set.invert keeps original keys as values';

my $bag-invert = bag(<a a b>).invert;
is-deeply pair-signatures($bag-invert), ("1\tb", "2\ta"), 'Bag.invert uses counts as keys';

my $mix-invert = {a => 2, b => 0.5}.Mix.invert;
is-deeply pair-signatures($mix-invert), ("0.5\tb", "2\ta"), 'Mix.invert uses weights as keys';
