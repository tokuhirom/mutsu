use Test;

# Slice F (env<->locals coherence): when a Junction argument auto-threads a
# function call, the call is dispatched once per eigenstate. Each eigenstate
# dispatch clears `pending_rw_writeback_sources` on entry, so a captured-outer
# variable mutated by the threaded body (`sub j($x) { $count++ }`) only had the
# final iteration's write recorded — and the auto-thread call site never drained
# it at all, relying on the reverse env->locals pull. With that pull disabled
# (MUTSU_NO_REVERSE_SYNC=1) the caller's slot stayed stale. This pins the fix:
# the auto-thread loop accumulates the captured-outer writes across all
# eigenstate calls and the call site writes the final env value through to the
# caller's local slot.

plan 8;

# A Junction in an untyped scalar param auto-threads; the body increments a
# captured-outer counter once per eigenstate.
{
    my $count = 0;
    sub j1($x) { $count++ }
    j1(3 | 4 | 5);
    is $count, 3, 'autothreaded ++ accumulates through to caller slot';
}

# Leading scalar param auto-threads even with a trailing slurpy.
{
    my $count = 0;
    sub j2($x, *@a) { $count++ }
    j2(3 | 4 | 5, 6);
    is $count, 3, 'autothread with trailing slurpy accumulates to caller slot';
}

# Compound += accumulation.
{
    my $sum = 0;
    sub j3($x) { $sum += $x }
    j3(1 | 10 | 100);
    is $sum, 111, 'autothreaded += sums all eigenstates into caller slot';
}

# A 2-element junction threads exactly twice.
{
    my $count = 0;
    sub j4($x) { $count++ }
    j4(1 | 2);
    is $count, 2, 'two-element junction threads twice';
}

# String append captured-outer write.
{
    my $trail = "";
    sub j5($x) { $trail ~= $x }
    j5("a" | "b" | "c");
    is $trail.comb.sort.join, "abc", 'autothreaded string append reaches caller slot';
}

# The auto-thread result value itself is still a Junction (unchanged behavior).
{
    my $count = 0;
    sub j6($x) { $count++; $x * 2 }
    my $r = j6(1 | 2 | 3);
    ok $r ~~ Junction, 'autothread still returns a Junction result';
    is $count, 3, 'side-effect counter still accumulates alongside the result';
}

# Two captured-outer vars mutated by the threaded body.
{
    my $n = 0;
    my $last = 0;
    sub j7($x) { $n++; $last = $x }
    j7(7 | 8 | 9);
    is $n, 3, 'first captured-outer var accumulates under autothread';
}
