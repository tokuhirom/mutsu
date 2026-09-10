use Test;

# A bare `Lock.protect` critical section must make a shared scalar's
# read-modify-write and a shared array element mutation visible to the next
# holder — the accumulator is genuinely shared across all threads, while the
# per-iteration loop lexical stays thread-local. Regression test for a bug
# where `Lock.protect`/`.lock`/`.unlock` never entered the critical-section
# tracking that `Semaphore.acquire`/`.release` already used (see
# t/semaphore-shared-scalar.t), so concurrent `Lock`-protected accumulation
# silently dropped or duplicated updates.

plan 3;

{
    my Lock $l .= new;
    my $r = 0;
    my @p;
    for ^200 {
        my $i = $_;
        @p.push: Promise.start({
            $l.protect({
                $r += $i;
            });
        });
    }
    await @p;
    is $r, (^200).sum, 'Lock.protect-protected scalar accumulation';
}

{
    my Lock $l .= new;
    my @r;
    my @p;
    for ^200 {
        my $i = $_;
        @p.push: Promise.start({
            $l.protect({
                @r[$i]++;
            });
        });
    }
    await @p;
    is-deeply @r, [1 xx 200], 'Lock.protect-protected array element mutation';
}

{
    my Lock $l .= new;
    my $r = 0;
    my @p;
    for ^200 {
        my $i = $_;
        @p.push: Promise.start({
            $l.lock;
            $r += $i;
            $l.unlock;
        });
    }
    await @p;
    is $r, (^200).sum, 'explicit Lock.lock/.unlock accumulation';
}
