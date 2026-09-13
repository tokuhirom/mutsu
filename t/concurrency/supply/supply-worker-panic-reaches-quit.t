use Test;

# Issue #8185: a failure raised by a Supply's *producer* code must reach the
# consumer's quit handler, and that has to hold for a Rust-level panic
# (overflow / index-OOB / capacity-overflow) just as much as for a `die`.
#
# Two gaps made it not hold. A panic raised on a detached `run_supply_act_loop`
# worker outside the VM's own `catch_unwind` frames unwound into the worker
# pool, whose `worker_loop` catches and discards it (ADR-0020) -- the tap simply
# went silent, with no diagnostic, no exception and no exit code. And even a
# plain `die` in a `supply { }` body consumed by `react`/`whenever` bypassed the
# subscription's QUIT phasers and killed the whole react as `X::React::Died`.
#
# The panic trigger below is the same one `t/vm/start-panic-boundary.t` uses:
# `@a[2**64 - 1] = 1` wraps `index + 1` to zero, so no slot is allocated and the
# store panics. A merely-large index is guarded by a fallible reservation and
# surfaces as a clean error instead, so it would not exercise this boundary.

plan 12;

# 1: a panic in a `supply { }` body reaches the consuming whenever's QUIT.
{
    my $quit-message;
    my @received;
    react {
        whenever supply { emit 1; my @a; @a[2**64 - 1] = 1; emit 2; } -> $v {
            @received.push($v);
            QUIT { $quit-message = .message; done }
        }
        whenever Promise.in(5) { done }
    }
    is @received, [1], 'values emitted before the panic are still delivered';
    ok $quit-message.defined, 'a supply-body panic reaches the QUIT phaser';
    ok $quit-message.contains('Internal error'),
        'the QUIT reason carries the internal-error prefix';
}

# 2: the same shape with a plain `die` (the panic path must not be a special
# case bolted beside a working die path -- before this fix neither worked).
{
    my $quit-message;
    react {
        whenever supply { emit 1; die "boom"; } -> $v {
            QUIT { $quit-message = .message; done }
        }
        whenever Promise.in(5) { done }
    }
    is $quit-message, 'boom', 'a supply-body die reaches the QUIT phaser';
}

# 3: a panic in a `whenever` body inside a `supply { }` block, driven by a
# detached act-loop worker over a live channel-backed source, reaches the
# downstream tap's `quit =>` handler.
{
    my $done-promise = Promise.new;
    my $quit-reason;
    my @received;
    my $src = supply {
        whenever Supply.interval(0.05) -> $i {
            emit $i;
            my @a; @a[2**64 - 1] = 1;
        }
    }
    my $tap = $src.tap(
        -> $v { @received.push($v) },
        quit => -> $e { $quit-reason = $e; $done-promise.keep(True) },
        done => -> { $done-promise.keep(False) },
    );
    await Promise.anyof($done-promise, Promise.in(5));
    ok $quit-reason.defined, 'a detached worker panic reaches the tap quit handler';
    ok $quit-reason.Str.contains('Internal error'),
        'the tap quit reason carries the internal-error prefix';
    is @received, [0], 'the value emitted before the panic still reached the tap';
    $tap.close;
}

# 4: same detached shape with a `die`.
{
    my $done-promise = Promise.new;
    my $quit-reason;
    my $src = supply {
        whenever Supply.interval(0.05) -> $i {
            emit $i;
            die "worker boom";
        }
    }
    my $tap = $src.tap(
        -> $v { },
        quit => -> $e { $quit-reason = $e; $done-promise.keep(True) },
        done => -> { $done-promise.keep(False) },
    );
    await Promise.anyof($done-promise, Promise.in(5));
    is $quit-reason.Str, 'worker boom',
        'a detached worker die reaches the tap quit handler';
    $tap.close;
}

# 5: a healthy supply is unaffected -- the new failure route must not turn an
# ordinary completion into a quit.
{
    my @received;
    my $quit-ran = False;
    react {
        whenever supply { emit 1; emit 2; done; } -> $v {
            @received.push($v);
            QUIT { $quit-ran = True }
        }
        whenever Promise.in(5) { done }
    }
    is @received, [1, 2], 'a healthy supply still delivers every value';
    nok $quit-ran, 'a healthy supply does not run the QUIT phaser';
}

# 6: a panic in an ordinary (non-Supply) pooled task still behaves as before --
# it breaks its Promise and `await` rethrows it, rather than being rerouted.
{
    my $caught;
    try {
        my $p = start { my @a; @a[2**64 - 1] = 1; };
        await $p;
        CATCH { default { $caught = .message } }
    }
    ok $caught.defined && $caught.contains('Internal error'),
        'a panic in a plain start{} still breaks its Promise';
}

# 7: an unhandled supply-body die (no QUIT phaser anywhere) still dies the
# react, exactly as before -- a supply failure must not become silent just
# because it now has a delivery route.
{
    my $died;
    try {
        react {
            whenever supply { die "unhandled boom" } -> $v { }
        }
        CATCH { default { $died = .message } }
    }
    ok $died.defined && $died.contains('unhandled boom'),
        'an unhandled supply-body die still dies the react';
}
