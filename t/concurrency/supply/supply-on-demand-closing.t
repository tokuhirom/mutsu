use v6;
use Test;

# `Supply.on-demand(..., closing => { ... })`: the `closing` callback runs when
# the supply is closed (it sends `done`, or its tap is closed).

plan 4;

# Synchronous body that emits then sends done.
{
    my $closed = 0;
    my $sod = Supply.on-demand:
        -> $s { $s.emit(42); $s.done },
        closing => { $closed++ };
    react whenever $sod -> $v { }
    is $closed, 1, 'closing fires when a synchronous on-demand supply is done';
}

# Asynchronous body (start block) that emits then sends done.
{
    my $closed = 0;
    my $sod = Supply.on-demand:
        -> $s { start { $s.emit(1); $s.done } },
        closing => { $closed++ };
    react {
        whenever $sod -> $v { }
        whenever Promise.in(0.5) { done }
    }
    ok $closed, 'closing fires for an async (start-block) on-demand supply';
}

# The emitted value still reaches the consumer before closing.
{
    my @got;
    my $closed = 0;
    my $sod = Supply.on-demand:
        -> $s { $s.emit('a'); $s.emit('b'); $s.done },
        closing => { $closed++ };
    react whenever $sod -> $v { @got.push($v) }
    is-deeply @got, ['a', 'b'], 'consumer sees all emitted values';
    is $closed, 1, 'closing fires once after the values';
}
