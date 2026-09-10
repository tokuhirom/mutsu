# A Rust-level panic triggered by user code (integer overflow, capacity
# overflow, index out of bounds in a native op, etc.) must NOT crash the whole
# process. The VM installs a panic->X:: boundary that converts such an
# otherwise-fatal panic into a catchable X::AdHoc exception (exit 1 + message),
# so it flows through the normal try/CATCH machinery.
#
# Stack overflow (`sub f { f() }; f()`) aborts rather than unwinds and is out of
# scope for this boundary.
use Test;

plan 9;

# 1-2: try {} catches the panic; the process survives and execution continues.
{
    my $survived = False;
    try {
        my @a;
        @a[2**64 - 1] = 1;   # "attempt to add with overflow" in the native op
    }
    $survived = True;
    ok $survived, 'try {} survives an overflow panic';
}

# A merely-large autoviv index is guarded by a fallible reservation
# (`autoviv_resize` -> `try_reserve`), so it surfaces as a clean catchable error
# rather than a Vec capacity-overflow panic. `try {}` survives it just the same.
{
    my $survived = False;
    try {
        my @a;
        @a[2**64 - 2] = 1;   # guarded: clean allocation error, not a panic
    }
    $survived = True;
    ok $survived, 'try {} survives a guarded huge-autoviv allocation failure';
}

# 3-4: the caught error is a real, introspectable exception.
{
    my $caught;
    try {
        my @a;
        @a[2**64 - 1] = 1;
        CATCH { default { $caught = $_ } }
    }
    ok $caught.defined, 'CATCH receives an exception object';
    isa-ok $caught, X::AdHoc, 'panic surfaces as X::AdHoc';
}

# 5: the message is preserved (prefixed with "Internal error:").
{
    my $msg;
    try {
        my @a;
        @a[2**64 - 1] = 1;
        CATCH { default { $msg = .message } }
    }
    ok $msg.contains('Internal error'), 'message carries the internal-error prefix';
}

# 6: $! is populated after a caught panic, like any other exception.
{
    try {
        my @a;
        @a[2**64 - 1] = 1;
    }
    isa-ok $!, X::AdHoc, '$! holds the converted exception';
}

# 7: dies-ok treats it as a throwing block.
dies-ok {
    my @a;
    @a[2**64 - 1] = 1;
}, 'dies-ok sees the panic as a thrown exception';

# 8: a panic nested deep inside loops/blocks within the try is still caught by
# the single try-body boundary (unwinding propagates through inner frames).
{
    my $count = 0;
    try {
        for ^3 {
            for ^3 {
                my @a;
                @a[2**64 - 1] = 1;
            }
        }
    }
    $count = 1;
    ok $count == 1, 'deeply-nested panic is caught by the enclosing try';
}

# 9: a normal (non-panic) lives block is unaffected by the boundary.
lives-ok {
    my @a;
    @a[3] = 1;
    @a[10] = 2;
}, 'ordinary array growth is unaffected';
