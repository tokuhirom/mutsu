use Test;
plan 8;

# IO::Handle.lock / .unlock using fcntl advisory record locks.
my $file = "tmp-lock-test-{$*PID}.txt";
spurt $file, "test content";
LEAVE { unlink $file }

# Shared lock on a read handle succeeds (returns True).
{
    my $fh = $file.IO.open(:r);
    is $fh.lock(:shared), True, 'shared lock on read handle';
    is $fh.unlock, True, '.unlock returns True';
    $fh.close;
}

# Exclusive lock on a write handle succeeds.
{
    my $fh = $file.IO.open(:w);
    is $fh.lock, True, 'exclusive lock on write handle';
    $fh.unlock;
    $fh.close;
}

# Exclusive lock on a read-only handle fails with X::IO::Lock (soft Failure).
{
    my $fh = $file.IO.open(:r);
    my $r = $fh.lock;   # default: exclusive
    isa-ok $r, Failure, 'exclusive lock on read handle returns a Failure';
    ok $r.exception ~~ X::IO::Lock, 'failure carries X::IO::Lock';
    $fh.close;
}

# Shared lock on a write-only handle fails with X::IO::Lock.
{
    my $fh = $file.IO.open(:w);
    my $r = $fh.lock(:shared);
    isa-ok $r, Failure, 'shared lock on write handle returns a Failure';
    $fh.close;
}

# A non-blocking exclusive lock fails while another process holds the lock.
{
    my $holder = $file.IO.open(:r);
    $holder.lock(:shared);
    start { sleep 1; $holder.unlock; $holder.close }
    # A separate process trying for an exclusive lock blocks until release,
    # then succeeds.
    is_run qq|my \$fh = '$file'.IO.open(:w); \$fh.lock; print "GOT"|,
        { :out<GOT>, :err('') },
        'write lock acquired after shared lock is released';
}

# Fire-and-forget start-thread output reaches stdout at program exit.
is_run 'start { say "FROM-START" }; sleep 1; say "FROM-MAIN"',
    { :out{ .contains('FROM-START') and .contains('FROM-MAIN') }, :err('') },
    'fire-and-forget start thread output is flushed at exit';
