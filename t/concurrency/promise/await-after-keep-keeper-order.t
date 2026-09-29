use v6;
use Test;

# ADR-0105 D2 / #10016: an `await` that reaches a promise only after a pool
# worker kept it still returns no earlier than that worker's next yield, so it
# observes the keeper's straight-line code exactly as an awaiter parked before
# the keep does. The awaiter below spins until the keep has happened, which
# makes the "late" path deterministic; before the fix every round printed the
# pre-assignment value. This is stronger than Rakudo, whose `await` on a kept
# promise returns at once (`$handle.already`): Rakudo 2026.07 usually prints
# 11111 and sometimes loses a round (10111), the same class as ADR-0105 D3's
# stronger-than-F2 rendezvous.

plan 4;

{
    my @seen;
    for ^5 {
        my $init = Promise.new;
        my $flag = 0;
        my $p = start {
            $init.keep;
            my $x = 0;
            $x++ for ^300;
            $flag = 1;
            sleep 0.05;
        };
        Nil until $init.status ~~ Kept;
        await $init;
        @seen.push: $flag;
        await $p;
    }
    is @seen.join, '11111', 'a late awaiter observes the keeper continuation up to its next yield';
}

# Several late awaiters on different threads are all released by the one yield.
{
    my $init = Promise.new;
    my $flag = 0;
    my $gate = Promise.new;
    my $p = start {
        await $gate;
        $init.keep(7);
        my $x = 0;
        $x++ for ^300;
        $flag = 1;
        sleep 0.05;
    };
    my @awaiters = (^3).map: -> $ {
        start {
            Nil until $init.status ~~ Kept;
            my $v = await $init;
            "$v:$flag";
        }
    };
    $gate.keep;
    is (await @awaiters).join(' '), '7:1 7:1 7:1', 'every late awaiter waits for the same keeper yield';
    await $p;
}

# The keeper awaiting its own promise is already ordered after its keep: no wait.
{
    my $p = start {
        my $own = Promise.new;
        $own.keep(42);
        await $own;
    };
    is await($p), 42, 'a keeper awaiting its own kept promise returns at once';
}

# A keeper that never yields still releases a late awaiter (the tick fallback).
{
    my $init = Promise.new;
    my $stop = False;
    my $p = start {
        $init.keep;
        my $x = 0;
        $x++ until $stop;
    };
    Nil until $init.status ~~ Kept;
    await $init;
    $stop = True;
    await $p;
    pass 'a late awaiter of a CPU-bound keeper is released without the keeper yielding';
}

# vim: expandtab shiftwidth=4
