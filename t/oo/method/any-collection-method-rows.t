use v6.e;
use Test;

plan 18;

my $value = 42;
is $value.elems, 1, 'Any.elems uses the scalar row';
is $value.end, 0, 'Any.end uses the scalar row';
is-deeply $value.keys.List, (0,), 'Any.keys uses the scalar row';
is-deeply $value.values.List, (42,), 'Any.values uses the scalar row';
is-deeply $value.kv.List, (0, 42), 'Any.kv uses the scalar row';
is-deeply $value.pairs.List, (0 => 42,), 'Any.pairs uses the scalar row';
is-deeply $value.antipairs.List, (42 => 0,), 'Any.antipairs uses the scalar row';
is-deeply $value.reverse.List, (42,), 'Any.reverse uses the scalar row';

my $text = 'abc';
is $text.elems, 1, 'the Str shape reaches Any.elems';
is-deeply $text.keys.List, (0,), 'the Str shape reaches Any.keys';
is-deeply $text.values.List, ('abc',), 'the Str shape reaches Any.values';
is-deeply $text.reverse.List, ('abc',), 'the Str shape reaches Any.reverse';

is-deeply (1, 2, 3).keys.List, (0, 1, 2), 'List.keys keeps its more-specific row';
is-deeply (1, 2, 3).values.List, (1, 2, 3), 'List.values keeps its more-specific row';
is-deeply ({a => 1, b => 2}).pairs.List.sort(*.key),
    (a => 1, b => 2), 'Map.pairs keeps its more-specific row';
is-deeply (1..3).reverse.List, (3, 2, 1), 'Range.reverse keeps the cascade path';
is-deeply (1, 2, 3).reverse.List, (3, 2, 1), 'List.reverse keeps its more-specific row';
is-deeply (1, 2, 3).antipairs.List, (1 => 0, 2 => 1, 3 => 2),
    'List.antipairs keeps its more-specific row';
