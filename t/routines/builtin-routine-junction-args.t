use v6;
use Test;

# Junction arguments of the builtin `elems` and `join` behave as rakudo's
# signatures make them: `sub elems($a)` autothreads; `join`'s slurpy takes a
# Junction element whole, and a list holding only that Junction joins to the
# Junction itself, while a Junction among other elements, or as the separator,
# threads the join (#18221).

plan 10;

is elems(any((1, 2), (3,))).raku, 'any(2, 1)', 'elems autothreads over a Junction';
is elems(any(1, 2)).raku, 'any(1, 1)', 'elems of scalar eigenstates';
is elems((1, 2, 3)), 3, 'elems of a plain list is unchanged';

is join(',', any(1, 2)).raku, 'any(1, 2)', 'join of one Junction is that Junction';
is (any(1, 2),).join(',').raku, 'any(1, 2)', '.join of a one-Junction list too';
is join(',', any(1, 2), 3).raku, 'any("1,3", "2,3")', 'a Junction among other elements threads';
is join(',', 5).raku, '"5"', 'a lone plain element is still stringified';

is join(any(',', ';'), 1, 2).raku, 'any("1,2", "1;2")', 'a Junction separator threads join';
is (1, 2).join(any(',', ';')).raku, 'any("1,2", "1;2")', 'and .join';
is [1, 2].join(all(',', ';')).raku, 'all("1,2", "1;2")', 'keeping the separator Junction kind';
