use Test;

# §D state-ownership: the VM constructs the `Lock` family natively (no
# interpreter `.new` bounce), behaving identically to the interpreter's
# `dispatch_new` arm.

plan 9;

# --- Lock ---
{
    my $l = Lock.new;
    isa-ok $l, Lock, 'Lock.new returns a Lock';
    ok $l.defined, 'Lock instance is defined';
    # each Lock gets a fresh id, so two locks are not equal
    my $l2 = Lock.new;
    isa-ok $l2, Lock, 'second Lock.new returns a Lock';
}

# --- Lock::Async ---
{
    my $la = Lock::Async.new;
    is $la.WHAT.^name, 'Lock::Async', 'Lock::Async.new WHAT name';
    ok $la.defined, 'Lock::Async instance is defined';
}

# --- Lock.protect runs the block (return value) ---
{
    my $l = Lock.new;
    my $r = $l.protect({ 41 + 1 });
    is $r, 42, 'Lock.protect returns the block value';
}

# --- Lock::Async.protect runs the block ---
{
    my $la = Lock::Async.new;
    my $r = $la.protect({ 7 * 6 });
    is $r, 42, 'Lock::Async.protect returns the block value';
}

# --- mutual exclusion: a guarded counter ends correct under a single thread ---
{
    my $l = Lock.new;
    my $sum = 0;
    for 1..10 {
        $sum = $l.protect({ $sum + 1 });
    }
    is $sum, 10, 'Lock.protect guards a running total';
}

# --- nesting Lock::Soft ---
{
    my $ls = Lock.new;
    my $r = $ls.protect({ $ls.protect({ 99 }) });
    is $r, 99, 'reentrant-style nested protect returns inner value';
}
