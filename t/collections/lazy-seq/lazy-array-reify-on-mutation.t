use Test;

# Reify-on-mutation for lazy `@`-arrays (ANALYSIS §F lazy-seq ④ / lazy-arrays.md L3).
# An element assignment / delete on a lazy array must materialize its backing
# first, so the other elements survive — previously a bare elem-assign
# autovivified a fresh empty Array and lost them.

plan 4;

# A finite `.lazy` list: elem-assign preserves the untouched elements.
{
    my @a = (1..5).lazy;
    @a[2] = 99;
    is-deeply @a[0..4].list, (1, 2, 99, 4, 5).list,
        'elem-assign on a finite lazy array keeps the other elements';
}

# An infinite lazy source: elem-assign reifies a prefix and keeps the elements.
{
    my @a = 1..*;
    @a[2] = 99;
    is-deeply @a[0..4].list, (1, 2, 99, 4, 5).list,
        'elem-assign on an infinite lazy array reifies the touched prefix';
}

# :delete on a lazy array leaves a hole, not a truncated/empty array.
{
    my @a = (1..5).lazy;
    @a[1]:delete;
    is @a[0], 1, ':delete on a lazy array keeps the leading element';
    is @a[2], 3, ':delete on a lazy array keeps the trailing elements';
}
