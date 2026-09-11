use Test;

plan 5;

my @a is List = 1, 2, 3;

is @a.^name, 'List', 'array declared with `is List` has List identity';
is @a.raku, '(1, 2, 3)', 'array declared with `is List` renders as a List';
isa-ok @a, List, 'array declared with `is List` is a List';
is @a, (1, 2, 3), '`is List` keeps declared values';
throws-like { @a = 4, 5, 6 }, X::Assignment::RO, '`is List` array cannot be reassigned';
