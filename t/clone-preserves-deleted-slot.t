# `.clone` copies the whole ArrayData, including per-slot metadata: a deleted
# slot's hole (`:exists` == False) survives the clone, and so do `is default`
# and shaped metadata. (roast/S02-types/array.t test 108, rakudo #1434.)
use Test;

plan 8;

# The deleted-slot :exists state is preserved through .clone.
{
    my @a = ^10;
    @a[3]:delete;
    is-deeply (@a[3]:exists), False, 'deleted slot :exists is False in the original';
    my @b := @a.clone;
    is-deeply (@b[3]:exists), False, 'deleted slot :exists is False in the clone too';
    is-deeply (@b[4]:exists), True, 'a live slot still exists in the clone';
    is @b[3].defined, False, 'the cloned deleted slot reads as an undefined hole';
}

# The clone is an independent copy: mutating one does not affect the other.
{
    my @a = 1, 2, 3;
    my @b = @a.clone;
    @b[0] = 99;
    is @a[0], 1, 'clone is an independent copy (mutating clone leaves original)';
    @a[1] = 77;
    is @b[1], 2, 'clone is an independent copy (mutating original leaves clone)';
}

# `is default` survives .clone.
{
    my @a is default(42) = 1, 2, 3;
    my @b = @a.clone;
    is @b[10], 42, 'is default survives .clone';
}

# A fresh delete on the clone does not leak back to the original.
{
    my @a = ^5;
    my @b = @a.clone;
    @b[2]:delete;
    is-deeply (@a[2]:exists), True, 'deleting in the clone does not affect the original';
}
