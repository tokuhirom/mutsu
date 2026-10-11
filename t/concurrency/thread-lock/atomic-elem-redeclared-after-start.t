use Test;

# #11833: an element atomic (`cas(@a[i], ...)`, `atomic-fetch-inc(@a[i])`,
# `cas(%h{k}, &code)`, ...) on an `@`/`%` declared AFTER the first `start`.
# Such a name is re-declared in the cross-thread store, so `GetLocal` reads the
# frame's own binding and skips the name-keyed atomic lane; the write used to
# go to that lane anyway and was lost. The element cell now lives in the
# binding the frame holds.

plan 12;

await start { 1 };

{
    my @d = 0, 0, 0;
    cas(@d[1], 0, 5);
    is-deeply @d, [0, 5, 0], 'cas on an array element declared after a start';
}

{
    my %h = a => 1;
    cas(%h<a>, 1, 7);
    is %h<a>, 7, 'cas on a hash element declared after a start';
    cas(%h<a>, { $_ + 10 });
    is %h<a>, 17, 'code-form cas on a hash element declared after a start';
}

{
    my @c = 1, 2;
    cas(@c[0], { $_ * 10 });
    is-deeply @c, [10, 2], 'code-form cas on an array element declared after a start';
}

{
    my atomicint @n = 0, 0;
    is atomic-fetch-inc(@n[0]), 0, 'atomic-fetch-inc answers the old element value';
    @n[1]⚛++;
    is-deeply @n.List, (1, 1), 'atomic-fetch-inc and ⚛++ land in the element';
    is @n.sum, 2, 'the elements read back through every path';
}

{
    my %g = x => 0;
    atomic-assign(%g<x>, 5);
    is %g<x>, 5, 'atomic-assign on a hash element declared after a start';
}

{
    my @md = [0, 0], [0, 0];
    cas(@md[1;0], 0, 9);
    is-deeply @md, [[0, 0], [9, 0]], 'multi-dimensional cas declared after a start';
}

sub f() {
    my @q = 0, 0, 0;
    cas(@q[1], 0, 5);
    @q
}
f();
is-deeply f(), [0, 5, 0], 'cas inside a sub called after a start, on every call';

# An inner `my @d` must not be confused with an outer one of the same name.
{
    my @d = 1, 1;
    {
        my @d = 9, 9;
        cas(@d[0], 9, 3);
        is-deeply @d, [3, 9], 'the inner same-named array is swapped';
    }
    is-deeply @d, [1, 1], 'the outer same-named array is untouched';
}
