use Test;

# From CSS::TagSet (CSS::Module.extend(..., |c) calling `c.hash`): a sigilless
# `|c` / `\c` parameter is the term `c`, even when a routine `c` is in scope.
plan 4;

my &c = -> $x { $x };
sub via-capture(|c) { c.hash.keys.sort.join(',') }
is via-capture(:a(1), :b(2)), 'a,b', '|c wins over my &c';

sub d($x) { $x }
sub via-sigilless(\d) { d.elems }
is via-sigilless((1, 2, 3)), 3, '\d wins over sub d';

sub via-capture2(|d) { d.elems }
is via-capture2(1, 2, 3), 3, '|d wins over sub d';

is d(7), 7, 'the outer sub is still callable';
