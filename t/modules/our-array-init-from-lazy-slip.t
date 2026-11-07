use v6;
use Test;

# `our @a = |(1..*)` stored the lazy tail as a one-element array whose element
# rendered as `(...)`, where `my @a = |(1..*)` kept the array lazy. Found with
# Math::Sequences (`our @A000027 is export = |ℕ`). Identical under raku.

plan 6;

our @a = |(1..*);
is @a[^4].raku, '(1, 2, 3, 4)', 'our array from a slipped infinite range';
ok @a.is-lazy, 'and it stays lazy';

my @m = |(1..*);
is @m[^4].raku, '(1, 2, 3, 4)', 'my array, same';

our @b = |(1..5);
is @b.raku, '[1, 2, 3, 4, 5]', 'a finite slipped range is still eager';

our @c = |(1..*), 9;
is @c[^3].raku, '(1, 2, 3)', 'a slip with a sibling';

our @d = (1..*);
ok @d.is-lazy, 'the unslipped form is lazy too';
