# A WhateverCode array index in a nested assignment (`%h<k>[*-0] = v`,
# `@a[0][*-1] = v`) is resolved against the inner container's current length
# before it becomes a key — a freshly-autovivified inner array is length 0, so
# `*-0` is index 0. Previously the unresolved WhateverCode stringified to a
# non-numeric key and the assignment was silently dropped.
use Test;

plan 6;

{
    my %h;
    %h<k>[*-0] = 42;
    is-deeply %h<k>, [42], 'hash-element [*-0] autovivifies element 0';
}
{
    my @a;
    @a[0][*-0] = 5;
    is-deeply @a[0], [5], 'array-in-array [*-0] autovivifies element 0';
}
{
    # *-0 on a non-empty inner array appends at the end.
    my %h;
    %h<k> = [1, 2, 3];
    %h<k>[*-0] = 9;
    is-deeply %h<k>, [1, 2, 3, 9], '[*-0] on a 3-element inner array appends';
}
{
    # *-1 on a non-empty inner array overwrites the last element.
    my %h;
    %h<k> = [1, 2, 3];
    %h<k>[*-1] = 99;
    is-deeply %h<k>, [1, 2, 99], '[*-1] overwrites the last element';
}
{
    # *-1 on a freshly-vivified (empty) inner array is out of range: no-op.
    my %h;
    %h<k>[*-1] = 5;
    is-deeply %h<k>, [], '[*-1] on an empty inner array is out of range (no store)';
}
{
    # A plain concrete index still works alongside.
    my %h;
    %h<k>[2] = 8;
    is-deeply %h<k>, [Any, Any, 8], 'concrete nested index still autovivifies';
}
