use Test;

# A parameter is a fresh per-invocation binding. Once a thread has run, the
# cross-thread shared store is active and keyed by bare name, so two routines
# that merely happen to share a parameter name aliased each other: the callee's
# argument replaced the caller's when the call went through a thread boundary.

plan 5;

{
    sub inner($desc) { $desc }
    sub outer($desc) { await start { inner("inner-value") }; $desc }
    is outer("outer-value"), 'outer-value',
        'a callee parameter does not clobber the caller of the same name';
}

{
    sub inner2($desc) { $desc }
    sub outer2() { my $desc = 'outer-value'; await start { inner2("inner-value") }; $desc }
    is outer2(), 'outer-value',
        "a callee parameter does not clobber the caller's `my` of the same name";
}

{
    sub inner3($desc) { $desc }
    sub outer3(&body, $desc) { body(); $desc }
    is outer3({ await start { inner3("inner-value") } }, 'outer-value'), 'outer-value',
        'the same, through a callback parameter';
}

# The shared store still does its job: a lexical the thread genuinely closes
# over is written through.
{
    my $shared = 'before';
    await start { $shared = 'after' };
    is $shared, 'after', 'a captured lexical still crosses the thread boundary';
}

# A parameter passed `is rw` still writes back to its caller's container.
{
    sub bump($n is rw) { $n = $n + 1 }
    my $count = 0;
    await start { 1 };
    bump($count);
    is $count, 1, 'an is-rw parameter still writes back after a thread has run';
}
