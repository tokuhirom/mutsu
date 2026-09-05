use Test;

# A `for` loop's MULTI-parameter list binds through a plain assignment, and
# `SetLocal` writes THROUGH a shared `ContainerRef` cell when the slot holds
# one. Two things put a cell there:
#
#   1. an outer lexical of the same name that was boxed before the loop, and
#   2. a closure created in the body, which boxes the parameter's own slot at
#      capture time.
#
# Only (1) was severed, and only once at loop entry, so every closure a
# multi-param loop created shared one cell and observed the LAST iteration's
# value. Silent: exit 0, the enclosing lexical still read correctly afterwards,
# only the closures were wrong.
#
# The sever now runs before every iteration, which is what makes each
# iteration's binding actually fresh. Every expectation below was measured
# against rakudo 2026.07.

plan 12;

# --- the headline: one closure per iteration ---------------------------------
{
    my $x = 1;                      # an enclosing `my` of the same name
    my @a = 10, 20, 30, 40;
    my @c;
    for @a -> $x, $y { @c.push(-> { $x }) }
    is-deeply @c>>.().Array, [10, 30], 'each iteration closes over its own value';
    is $x, 1, 'and the enclosing lexical is untouched';
}

{
    my @a = 10, 20, 30, 40;
    my @c;
    for @a -> $x, $y { @c.push(-> { $y }) }
    is-deeply @c>>.().Array, [20, 40], 'the second parameter likewise';
}

# An already-closed sibling block's same-named lexical is enough to collide.
{
    { my $x = 5; }
    my @a = 10, 20, 30, 40;
    my @c;
    for @a -> $x, $y { @c.push(-> { $x }) }
    is-deeply @c>>.().Array, [10, 30], 'a popped sibling block collides too';
}

# No same-named lexical anywhere: this always worked, and must keep working.
{
    my @a = 1, 2, 3, 4;
    my @c;
    for @a -> $p, $q { @c.push(-> { "$p/$q" }) }
    is-deeply @c>>.().Array, ['1/2', '3/4'], 'no collision needed for it to be right';
}

# --- an outer lexical that is GENUINELY mutated keeps its cell ----------------
# This is the residue that defeated the earlier attempts at this bug: excluding
# the loop's bind from the mutation scan is not enough, because a real write
# elsewhere in the unit re-earns the cell. Severing the binding per iteration
# leaves that cell alone -- the closure that captured it still sees the write.
{
    my @a = 1, 2, 3, 4;
    my $x = 1;
    my $c = -> { $x };
    $x = 2;
    for @a -> $x, $y { }
    is $c(), 2, 'a genuine outer mutation is still observed through its cell';
}

{
    my @a = 1, 2, 3, 4;
    my $x = 1;
    my $c = -> { $x };
    $x = 2;
    my @inner;
    for @a -> $x, $y { @inner.push(-> { $x }) }
    is $c(), 2, '... even when the loop also makes closures';
    is-deeply @inner>>.().Array, [1, 3], '... and those closures are still fresh';
}

# --- the single-parameter form is bound natively and was always right --------
{
    my $z = 1;
    my @b = 10, 20, 30;
    my @cs;
    for @b -> $z { @cs.push(-> { $z }) }
    is-deeply @cs>>.().Array, [10, 20, 30], 'the single-parameter form is unchanged';
}

# --- nested same-named multi-param loops --------------------------------------
{
    my @out;
    for 1, 2, 3, 4 -> $p, $q {
        for 7, 8 -> $p, $q { @out.push("$p-$q") }
        @out.push("outer $p-$q");
    }
    is-deeply @out, ['7-8', 'outer 1-2', '7-8', 'outer 3-4'],
        'an inner loop does not leak into the outer one';
}

# --- `@`/`%` parameters bind the container, and that aliasing is the point ----
{
    my @a = [1, 2], [3, 4];
    my @seen;
    for @a -> @row { @seen.push(@row.join(',')) }
    is-deeply @seen, ['1,2', '3,4'], 'an @-parameter still binds its container';
}

{
    my @pairs = 'a', 1, 'b', 2;
    my $sum = 0;
    for @pairs -> $k, $v { $sum += $v }
    is $sum, 3, 'an ordinary multi-param loop body still sees each value';
}
