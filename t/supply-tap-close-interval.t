use v6;
use Test;

# Pin for the Tap.close act-loop teardown: closing a tap over a
# channel-backed interval source must actually stop the worker driving it.
# Before the fix the worker (and the interval-timer entry feeding it) ran
# until process exit — roast/S17-supply/syntax.t test 63 leaked ~4000 such
# workers and burned ~610 CPU-seconds.
#
# Deterministic (not load-flaky) because run_supply_act_loop re-checks the
# close flag after every receive: once .close returns, no new body dispatch
# can start. The 0.35s grace covers the bounded 250ms wait plus any body
# already in flight.

plan 2;

{
    my atomicint $ticks = 0;
    my $s = supply { whenever Supply.interval(0.01) { $ticks⚛++ } };
    my $tap = $s.tap({ ; });
    sleep 0.1;
    $tap.close;
    sleep 0.35;            # > one 250ms bounded-wait round + in-flight body
    my $after = ⚛$ticks;
    sleep 0.3;
    is ⚛$ticks, $after, 'supply-block interval stops ticking after tap.close';
}

{
    my atomicint $n = 0;
    my $tap = Supply.interval(0.01).tap({ $n⚛++ });
    sleep 0.1;
    $tap.close;
    sleep 0.35;
    my $after = ⚛$n;
    sleep 0.3;
    is ⚛$n, $after, 'direct interval tap stops ticking after tap.close';
}
