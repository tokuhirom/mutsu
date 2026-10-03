use Test;

# A value-adverb `:delete` (`:v:delete`, `:k:delete`, `:p:delete`,
# `:kv:delete`) leaves the same hole a plain `:delete` does: the slot no
# longer exists, and reads as the container default (#11320).

plan 9;

{
    my @a = 10, 20, 30;
    my $v = @a[0]:v:delete;
    is $v, 10, ':v:delete returns the value';
    nok @a[0]:exists, ':v:delete leaves a slot that does not exist';
    is-deeply @a[0], Any, 'the hole reads as Any';
}

{
    my @a = 10, 20, 30;
    @a[1]:p:delete;
    nok @a[1]:exists, ':p:delete leaves a hole';
    @a[0]:k:delete;
    nok @a[0]:exists, ':k:delete leaves a hole';
    is @a.elems, 3, 'deleting inner slots keeps the length';
}

{
    my @a is default(42) = 1, 2, 3;
    @a[0]:kv:delete;
    nok @a[0]:exists, ':kv:delete leaves a hole';
    is @a[0], 42, 'the hole reads as the array default';
}

{
    my @a = 1, 2, 3;
    my @b := @a;
    @a[1]:v:delete;
    nok @b[1]:exists, 'the hole is visible through a binding';
}
