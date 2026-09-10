# A Rust-level panic (integer overflow, capacity overflow, index OOB, ...)
# triggered by user code inside a `hyper`/`race` worker thread must NOT crash the
# process or leak a raw Rust panic message: each per-item call runs under the same
# panic->X::AdHoc boundary `start{}`/Promise workers use (see start-panic-boundary.t).
# The panic surfaces as a catchable X::AdHoc carrying the internal-error message.
use Test;

plan 6;

# 1-2: a panicking hyper.map is catchable (no crash/hang) and is an X::AdHoc.
{
    my $caught;
    try {
        my @r = (1..2).hyper.map({ my @a; @a[2**64 - 1] = 1; 1 });
        CATCH { default { $caught = $_ } }
    }
    ok $caught.defined, 'hyper.map worker panic surfaces as a catchable exception';
    ok $caught ~~ X::AdHoc,
        'the converted exception is an X::AdHoc';
}

# 3: the message carries the internal-error prefix (uniform with start{}).
{
    my $msg;
    try {
        my @r = (1..2).race.map({ my @a; @a[2**64 - 1] = 1; 1 });
        CATCH { default { $msg = .message } }
    }
    ok $msg.defined && $msg.contains('Internal error'),
        'race.map worker panic message carries the internal-error prefix';
}

# 4: dies-ok sees the panic as a thrown exception.
dies-ok {
    my @r = (1..2).hyper.map({ my @a; @a[2**64 - 1] = 1; 1 });
}, 'dies-ok sees the hyper.map worker panic as a throw';

# 5-6: ordinary hyper/race are unaffected by the boundary.
is (1..5).hyper.map(* * 2).List, (2, 4, 6, 8, 10),
    'ordinary hyper.map still produces correct results';
is (1..5).race.map(* + 1).sort.List, (2, 3, 4, 5, 6),
    'ordinary race.map still produces correct results';
