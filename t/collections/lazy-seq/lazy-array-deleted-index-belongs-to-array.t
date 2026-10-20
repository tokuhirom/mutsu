use Test;

# `@a[i]:delete` on a lazy array keeps it lazy, and the deleted index reads as
# missing to `:exists`. The record belongs to the array (ADR-0097; it was the
# name-keyed `__mutsu_deleted_index::` env key): a fresh array of the same
# name starts without holes, and every holder of the array sees them.

plan 10;

my @a = 1, 2, 4 ... Inf;
@a[2]:delete;
nok @a[2]:exists, 'a deleted index of a lazy array does not exist';
ok @a[3]:exists, '... its neighbour still does';
is @a[3], 8, '... and the sequence goes on';

@a[2] = 7;
ok @a[2]:exists, 'assigning the index again makes it exist';

sub fresh { my @l = 1, 3 ... Inf; @l[0]:delete; @l[0]:exists }
is-deeply (fresh(), fresh()), (False, False), 'each call deletes from its own array';

{ my @l = 1, 3 ... Inf; @l[5]:delete; }
{ my @l = 1, 3 ... Inf; ok @l[5]:exists, 'a same-named array in a sibling block has no hole' }

my @z = 1, 3 ... Inf;
my @w := @z;
@z[1]:delete;
nok @w[1]:exists, 'a bound alias sees the hole';

my @c = 1, 2, 3;
@c[1]:delete;
nok @c[1]:exists, 'a reified array keeps its own hole';

my @d is default(99) = 1 ... *;
@d[2]:delete;
nok @d[2]:exists, 'a deleted index of a lazy array with a default does not exist';
is @d[2], 99, '... and reads as the default';
