# A Rust-level panic (integer overflow, capacity overflow, index OOB, ...)
# triggered by user code inside a `start{}`/Promise worker thread must NOT hang
# the awaiter or crash the whole process. The worker installs the same
# panic->X:: boundary the main thread uses (see vm-panic-boundary.t), converting
# such a panic into a broken Promise whose cause is a catchable X::AdHoc, so
# `await` rethrows it through the normal exception machinery.
use Test;

plan 6;

# 1: awaiting a worker that panics rethrows (does not hang) and is catchable.
{
    my $survived = False;
    my $caught;
    try {
        my $p = start { my @a; @a[2**64 - 1] = 1; "unreachable" };
        await $p;
        CATCH { default { $caught = $_ } }
    }
    $survived = True;
    ok $survived, 'await of a panicking start{} survives (no hang/crash)';
    ok $caught.defined, 'the panic surfaces as a catchable exception';
}

# 2: the converted exception is an X::AdHoc carrying the internal-error message.
{
    my $msg;
    try {
        my $p = start { my @a; @a[2**64 - 2] = 1; };  # capacity-overflow panic
        await $p;
        CATCH { default { $msg = .message } }
    }
    ok $msg.defined && $msg.contains('Internal error'),
        'message carries the internal-error prefix';
}

# 3: the Promise itself ends up Broken, not stuck.
{
    my $p = start { my @a; @a[2**64 - 1] = 1; };
    # await inside try so the broken promise does not abort the program
    try { await $p }
    is $p.status, Broken, 'a panicking start{} leaves the Promise Broken';
}

# 4: a normal (non-panic) start{} is unaffected by the boundary.
{
    my $p = start { 40 + 2 };
    is await($p), 42, 'ordinary start{} still resolves normally';
}

# 5: dies-ok sees the awaited panic as a thrown exception.
dies-ok {
    my $p = start { my @a; @a[2**64 - 1] = 1; };
    await $p;
}, 'dies-ok sees the awaited worker panic as a throw';
