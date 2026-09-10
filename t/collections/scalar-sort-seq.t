use Test;

# `.sort` on any non-list value is `self.list.sort`: a one-element Seq of the
# value itself. So "cba".sort is ("cba",).Seq, NOT the sorted characters, and
# 42.sort is (42,).Seq. mutsu previously returned the scalar unchanged.
# See raku-doc/doc/Language/traps.rakudoc (the "sort a string" trap).

plan 8;

is "cba".sort.elems, 1, 'Str.sort is a one-element list';
is "cba".sort.raku, '("cba",).Seq', 'Str.sort wraps the whole string';
is 42.sort.raku, '(42,).Seq', 'Int.sort is a one-element Seq';
is (1/2).sort.raku, '(0.5,).Seq', 'Rat.sort is a one-element Seq';
is (1 => 2).sort.raku, '(1 => 2,).Seq', 'Pair.sort is a one-element Seq';

# The documented idiom to actually sort a string's characters still works.
is "cba".comb.sort.join, 'abc', 'comb.sort.join sorts the characters';

# List / hash sorting is unaffected.
is (3, 1, 2).sort.join(','), '1,2,3', 'list sort still sorts';
is %(b => 2, a => 1).sort.raku, '(:a(1), :b(2)).Seq', 'hash sort still sorts pairs';
