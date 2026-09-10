use v6;
use Test;

# `Supply.on-demand(..., closing => { ... })` tapped DIRECTLY with `.tap`
# (not via `react whenever`): the `closing` callback must still fire when the
# supply closes. Previously the `.tap` path ignored the tapped supply's own
# `on_close_callbacks`, so `closing` never ran.

plan 4;

# Synchronous body that emits then sends done, tapped with `.tap`.
{
    my $closed = 0;
    my @got;
    my $sod = Supply.on-demand:
        -> $s { $s.emit(42); $s.done },
        closing => { $closed++ };
    $sod.tap(-> $v { @got.push($v) });
    is-deeply @got, [42], 'tap consumer sees the emitted value';
    is $closed, 1, 'closing fires for a synchronous on-demand supply tapped with .tap';
}

# Asynchronous body (start block), tapped with `.tap`.
{
    my $closed = 0;
    my $sod = Supply.on-demand:
        -> $s { start { $s.emit(1); $s.done } },
        closing => { $closed++ };
    $sod.tap(-> $v { });
    # Wait for the condition, not for a fixed duration: the body runs on a
    # worker thread, so a hardcoded `sleep` is a bet on scheduler latency that a
    # loaded CI runner loses. Poll up to 10s, but return as soon as it fires.
    for ^1000 {
        last if $closed;
        sleep 0.01;
    }
    ok $closed, 'closing fires for an async (start-block) on-demand supply tapped with .tap';
}

# Two values then done, tapped with `.tap`.
{
    my @got;
    my $n = 0;
    my $sod = Supply.on-demand:
        -> $s { $s.emit('a'); $s.emit('b'); $s.done },
        closing => { $n++ };
    $sod.tap(-> $v { @got.push($v) });
    is-deeply @got, ['a', 'b'], 'tap consumer sees all emitted values';
}
