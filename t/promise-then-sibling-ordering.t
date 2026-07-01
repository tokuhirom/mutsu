use Test;

# Regression test for the root cause behind roast S17-promise/then.t's
# intermittent CI-only failure ("simple keep"/"simple break"): `.then`/
# `.andthen`/`.orelse` registered on a not-yet-resolved Promise used to each
# spawn their own OS thread that blocked on the promise, then raced every
# other such thread to wake from the same `Condvar::notify_all` once
# `.keep`/`.break` fired. Two independent callbacks on the same promise (an
# `.then` alongside the head of an `.andthen` chain, as in the roast test)
# therefore had no ordering guarantee and could run in either order
# depending on OS scheduling -- rare locally, much more likely under CI's
# parallel test load.
#
# Fix: registrations on an unresolved promise are queued and drained by
# whichever call resolves it, all on one dedicated thread, in registration
# order. This test locks that ordering guarantee in directly (rather than
# trying to statistically reproduce the race), and loops many iterations so
# a regression back to the old "each registration is its own racing thread"
# behavior would show up as sporadic failures rather than a single lucky
# pass.

plan 1;

my $iterations = 200;
my $all-ok = True;

for ^$iterations {
    my $lock = Lock.new;
    my @order;
    my $p1 = Promise.new;

    # Two siblings registered directly on the still-Planned $p1, in this
    # source order: an independent `.then`, and the head of a 3-hop
    # `.andthen` chain. `$p2` is the *tail* of that chain (mirrors roast's
    # "independent-then" alongside the "andthen1 -> andthen2 -> ... ->
    # final-then" chain). The parenthesized-block form is required here:
    # chaining `.andthen: {...}.andthen: {...}` (colon-call form) is a
    # separate, pre-existing Raku parsing ambiguity (colon-call binds the
    # block loosely, so the second `.andthen` ends up called on the first
    # block's *return value* instead of the promise it returns) -- the roast
    # test itself uses the parenthesized form for exactly this reason.
    $p1.then: { $lock.protect: { @order.push: 'independent' } };
    my $p2 = $p1
        .andthen({ $lock.protect: { @order.push: 'chain-1' }; 1 })
        .andthen({ $lock.protect: { @order.push: 'chain-2' }; 1 })
        .then({ $lock.protect: { @order.push: 'chain-3' } });

    $p1.keep(1);
    await $p2;

    # `chain-3` resolving guarantees `chain-1` (queued second on $p1) has
    # already run, which -- because both siblings drain from $p1's *single*
    # queue in registration order -- guarantees `independent` (queued first)
    # ran before it, not merely "eventually, whenever its own thread got
    # scheduled" (the pre-fix behavior).
    unless @order.elems == 4 && @order[0] eq 'independent' {
        $all-ok = False;
        last;
    }
}

ok $all-ok, "sibling .then callbacks on the same promise run in registration order ({$iterations}x)";
