use Test;

# An infinite `gather` whose `take`s all live inside a routine the gather body
# CALLS must still suspend: the pull driver cannot suspend at the take itself
# (it can only snapshot its own frame), but the enclosing condition-driven loop
# in the gather body's own frame is a sound suspension point one boundary later.
#
# Before the fix the driver kept collecting eagerly and never stopped, so any of
# these hung forever. Found via the ecosystem parity sweep: EuclideanRhythm's
# `method list() { gather { loop { self!bitmap(...) } } }` (issue #7995).

plan 9;

# 1-2: bare sub called from an infinite `loop` in the gather body.
{
    sub gen() { take 1; take 2 }
    my $s = gather { loop { gen() } };
    is-deeply $s[^5].List, (1, 2, 1, 2, 1), 'infinite gather + take inside a called sub is lazy';
    is-deeply $s[^2].List, (1, 2), 'a second, shorter pull of the same gather agrees';
}

# 3: private method, the EuclideanRhythm shape.
{
    class C {
        method !gen() { take 'a'; take 'b' }
        method list() { gather { loop { self!gen() } } }
    }
    is-deeply C.new.list[^5].List, ('a', 'b', 'a', 'b', 'a'),
        'infinite gather + take inside a private method is lazy';
}

# 4: `while` loop rather than a bare `loop`.
{
    sub gen($n) { take $n }
    my $i = 0;
    my $s = gather { while True { gen(++$i) } };
    is-deeply $s[^4].List, (1, 2, 3, 4), 'infinite `while` gather + take in a called sub is lazy';
}

# 5: the take is nested two calls deep.
{
    sub inner() { take 'x' }
    sub outer() { inner() }
    my $s = gather { loop { outer() } };
    is-deeply $s[^3].List, ('x', 'x', 'x'), 'take two call levels below the gather body is lazy';
}

# 6: a loop INSIDE the callee is not a sound suspension point, but the gather
# body's own loop still is, so the pull terminates (it may over-produce).
{
    sub gen() { for 1..3 { take $_ } }
    my $s = gather { loop { gen() } };
    is-deeply $s[^4].List, (1, 2, 3, 1), 'take inside a callee loop still terminates the pull';
}

# 7-8: a FINITE gather with takes in a called sub keeps its exact contents —
# the deferred-suspension flag must not truncate or leak.
{
    sub gen($n) { take $n; take $n * 10 }
    my @got = gather { gen($_) for 1..3 };
    is-deeply @got.List, (1, 10, 2, 20, 3, 30), 'finite gather with nested takes is complete';
    my $s = gather { my $i = 0; while $i++ < 3 { gen($i) } };
    is-deeply $s.List, (1, 10, 2, 20, 3, 30), 'finite `while` gather with nested takes is complete';
}

# 9: the flag must not leak out of one pull into an unrelated later one.
{
    sub gen() { take 7 }
    my $a = gather { loop { gen() } };
    $a[^2];
    my @b = gather { gen(); gen() };
    is-deeply @b.List, (7, 7), 'a suspended pull does not truncate a later unrelated gather';
}
