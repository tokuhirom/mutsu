# An exported lazy `our @A = LIST ... *` is one list for the exporter and its importers (#17035):
# a generator that reads the array it builds must see the list the importer indexes, not a copy.
use lib 't/lib';
use Test;
use LazyOurArrayShared;
plan 3;
is @A[^6].join(' '), '0 1 1 4 26 236', 'a self-referencing exported sequence yields the right values';
is @Steps[^5].raku, '(0, 1, 3, 6, 10)', 'the generator is stepped once per element';
is @A[7], 39208, 'a later element after the first pull';
