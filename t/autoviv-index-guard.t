use Test;

# An absurd array index used in an auto-vivifying write must throw a catchable
# exception instead of aborting the process via an unrecoverable allocation
# failure (`handle_alloc_error`). raku itself aborts here with a MoarVM panic;
# mutsu is more graceful by guarding the reservation before it is attempted.

plan 13;

# Single-index assignment: the write fails (returns Nil/undef) rather than
# aborting, and the process keeps running.
{
    my @a;
    my $r = try { @a[9999999999999] = 1 };
    ok !$r.defined, 'absurd single-index autoviv fails gracefully';
    ok True, 'survived single-index absurd autoviv';
}

# The failure is catchable (a defined exception is set).
{
    my @a;
    my $caught;
    try { @a[9999999999999] = 1; CATCH { default { $caught = .message } } }
    ok $caught.defined, 'absurd single-index autoviv is catchable';
    like $caught, /allocation/, 'error mentions allocation failure';
}

# Nested autoviv.
{
    my @a;
    try { @a[0][9999999999999] = 1 }
    ok True, 'survived nested absurd autoviv';
}

# Post-increment autoviv.
{
    my @a;
    try { @a[9999999999999]++ }
    ok True, 'survived absurd post-increment autoviv';
}

# Slice autoviv.
{
    my @a;
    try { @a[1, 9999999999999] = 1, 2 }
    ok True, 'survived absurd slice autoviv';
}

# Normal autoviv still works after the guarded path.
{
    my @a;
    @a[5] = 42;
    is @a.elems, 6, 'normal small autoviv extends the array';
    is @a[5], 42, 'normal small autoviv stores the value';
}

# A moderately large but allocatable index still works.
{
    my @a;
    @a[1_000_000] = 7;
    is @a.elems, 1_000_001, 'moderately large autoviv works';
}

# String repetition with an absurd count is the same allocation-abort class and
# must also be catchable rather than aborting the process.
{
    my $caught;
    try { my $s = "x" x 99999999999999; CATCH { default { $caught = .message } } }
    ok $caught.defined, 'absurd string repetition is catchable';
    like $caught, /allocation/, 'string repeat error mentions allocation failure';
    is "ab" x 3, "ababab", 'normal string repetition still works';
}
