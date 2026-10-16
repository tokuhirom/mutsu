use Test;

# The first `cas` on an element of an `@`/`%` variable moves the variable onto
# a celled atomic store. When that first `cas` runs inside a closure another
# routine calls, the store reaches the declaring block (and every closure that
# captured the variable) through the closure's capture cell, not through a
# frame's return merge (ADR-12529 §3, slice 31). Checked against rakudo.

plan 7;

sub run(&c) { c() }

{
    my Int @c;
    @c[0] = 1;
    run { try cas(@c[0], -> $v { "bad" }) };
    is @c[0], 1, 'a rejected code-form cas in a closure leaves the element';
    cas(@c[0], -> $v { $v + 10 });
    is @c[0], 11, 'a later code-form cas in the block is seen by the block';
}

{
    my @a = 1, 2;
    run { cas(@a[1], 2, 20) };
    is-deeply @a, [1, 20], 'a 3-arg array cas in a closure';
    my &peek = { @a[1] };
    cas(@a[1], 20, 30);
    is peek(), 30, 'a closure made afterwards sees the next cas';
}

{
    my Int %h;
    %h<k> = 1;
    run { cas(%h<k>, -> $v { $v + 1 }) };
    is %h<k>, 2, 'a code-form hash cas in a closure';
    cas(%h<k>, -> $v { $v + 10 });
    is %h<k>, 12, 'and a later one in the block';
}

{
    my @m = [1, 2], [3, 4];
    run { cas(@m[1;1], -> $v { $v + 100 }) };
    is @m[1;1], 104, 'a multidim code-form cas in a closure';
}
