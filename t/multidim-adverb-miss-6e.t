use v6.e.PREVIEW;
use Test;

# 6.e reports `Nil` for every kind of multidim `:v`/`:k`/`:p` miss -- an
# in-bounds Array hole, an out-of-range coordinate, a missing Hash key alike.
# 6.d and earlier report the empty list `()` for all of them
# (t/typed-array-hole-adverbs.t pins that side). `:kv` is `()` under both.

plan 8;

{
    my @c;
    @c[0;1] = 5;
    is-deeply (@c[0;0]:v), Nil, ':v on an in-bounds hole is Nil';
    is-deeply (@c[0;0]:k), Nil, ':k on an in-bounds hole is Nil';
    is-deeply (@c[0;0]:p), Nil, ':p on an in-bounds hole is Nil';
    is-deeply (@c[0;0]:kv), (), ':kv on a hole is still the empty list';
    is-deeply (@c[0;1]:v), 5, ':v on an assigned slot is the value';
    is-deeply (@c[0;0]:!v), Any, ':!v on a hole is Any';
}

{
    my @a = [[[42, 666], ], ];
    is-deeply (@a[0;0;3]:k), Nil, ':k on an out-of-range coordinate is Nil';
}

{
    my %h;
    %h{"a";"b"} = 1;
    is-deeply (%h{"x";"y"}:v), Nil, ':v on a missing Hash key is Nil';
}
