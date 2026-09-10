use Test;

plan 11;

# In a signature BINDING (`:=`) a plain `@x` binds ONE positional argument; only
# an explicit `*@rest` is slurpy. mutsu treated a *trailing* `@x` as implicitly
# slurpy, so the last array target came back wrapped in an extra layer:
# `my (@a, @b) := (@x, @y)` bound `@b` to `(@y,)` instead of `@y`.

{
    my @x = 1, 2;
    my @y = 5;
    my (@a, @b) := (@x, @y);
    is-deeply @a.List, (1, 2), 'first array target binds its element';
    is-deeply @b.List, (5,), 'trailing array target binds its element, not a wrapper';
}

{
    my @x = 1, 2;
    my @y;
    my (@a, @b) := (@x, @y);
    is-deeply @b.List, (), 'an empty trailing array binds as empty';
}

{
    my @x = 1, 2;
    my @y = 5;
    my @z = 7, 8;
    my (@a, @b, @c) := (@x, @y, @z);
    is-deeply @c.List, (7, 8), 'the last of three array targets binds its element';
}

{
    my @y = 5;
    my (@b) := (@y,);
    is-deeply @b.List, (5,), 'a lone array target binds its element';
}

# An explicit `*@rest` is still slurpy.
{
    my ($x, @y, *@r) := (42, [13, 17], 5, 6, 7);
    is $x, 42, 'scalar target binds first element';
    is-deeply @y.List, (13, 17), 'plain array target binds one element';
    is-deeply @r.List, (5, 6, 7), 'an explicit *@rest is slurpy';
}

# The shape this was found through: `.classify` returns only the keys that
# matched, and the named-binding form must leave the other target empty.
{
    my (:@now, :@future) := (1, 2, 3).classify({ 'now' });
    is-deeply @now.List, (1, 2, 3), 'classify: the matched key binds its list';
    is-deeply @future.List, (), 'classify: the absent key binds empty';
}

# List ASSIGNMENT (`=`) keeps its greedy semantics: the FIRST @ slurps.
{
    my ($a, @b, $c) = 1, 2, 3, 4;
    is-deeply ($a, @b.List, $c), (1, (2, 3, 4), Any),
        'list assignment is still greedy at the first @ target';
}

# vim: expandtab shiftwidth=4
