use Test;

# `splice`'s start/elems positions take `Int`, `Whatever` or `Callable`, and a
# from-the-end index (`*-1`) is a `Callable` -- a `WhateverCode`. The lvalue
# path resolved it against the array's length; the BY-VALUE invocant path (a
# function result, an element read, a literal) did not, so the callable read as
# index 0 and `f().splice(*-1, 1)` cut the array's FIRST element.
#
# Crane reaches it as `Crane::At.at($root, @path).splice($step, 1)` with
# `$step` a `*-1` handed down from the caller's path.

plan 9;

sub at-rw($c) is rw { return-rw $c<k> }

{
    my %h = :k([1, 2, 3, 4]);
    at-rw(%h).splice(*-1, 1);
    is-deeply %h<k>, [1, 2, 3], 'a *-1 start on a call-result invocant';
}
{
    my %h = :k([1, 2, 3, 4]);
    at-rw(%h).splice(*-2, 1);
    is-deeply %h<k>, [1, 2, 4], 'a *-2 start on a call-result invocant';
}
{
    my %h = :k([1, 2, 3, 4]);
    at-rw(%h).splice(*-0, 0, 9);
    is-deeply %h<k>, [1, 2, 3, 4, 9], 'a *-0 start appends';
}
{
    my %h = :k([1, 2, 3, 4]);
    # The count callable is called with the number of elements still available
    # after the start (here 3), so `*-2` is 1.
    my $removed = at-rw(%h).splice(1, *-2);
    is-deeply $removed, [2], 'a callable ELEMS count resolves against the remainder';
    is-deeply %h<k>, [1, 3, 4], 'and removes exactly that many';
}
{
    my %h = :k([1, 2, 3, 4]);
    at-rw(%h).splice(2, 1);
    is-deeply %h<k>, [1, 2, 4], 'a plain Int start still works';
}

# The named-variable (lvalue) spelling was already right; keep the two in step.
{
    my @a = 1, 2, 3, 4;
    @a.splice(*-1, 1);
    is-deeply @a, [1, 2, 3], 'the named spelling (unchanged)';
}
{
    my @a = 1, 2, 3, 4;
    my $step = *-1;
    @a.splice($step, 1);
    is-deeply @a, [1, 2, 3], 'with the WhateverCode in a variable (unchanged)';
}
{
    my @a = 1, 2, 3, 4;
    sub through(\c, :$step!) { c.splice($step, 1); |c }
    through(@a, :step(*-1));
    is-deeply @a, [1, 2, 3], 'passed down through a routine (unchanged)';
}
