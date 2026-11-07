# A module whose last statement declares a lazy `@` array must load without forcing it (#17035).
use lib 't/lib';
use Test;
use LazyOurArrayFib;
use LazyOurArraySelf;
use LazyOurArrayMy;
plan 4;
is @F[^6].raku, '(1, 1, 2, 3, 5, 8)', 'a module loads with a lazy `our @` array as its last statement';
is @S[^6].raku, '(1, 2, 4, 7, 12, 20)', 'a generator that reads its own array';
is first-ten().raku, '(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)', 'a lazy `my @` inside an exported sub';
is @F[10], 89, 'a later element';
