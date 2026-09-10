use Test;

# A bare Semaphore critical section (`$s.acquire; ...; $s.release`) must make a
# shared scalar's read-modify-write and a shared array element mutation visible
# to the next holder — the accumulator is genuinely shared across all threads,
# while the per-iteration loop lexical stays thread-local.

plan 2;

{
    my Semaphore $s .= new(1);
    my $r = 0;
    my @p;
    for ^200 {
        my $i = $_;
        @p.push: Promise.start({
            $s.acquire;
            $r += $i;
            $s.release;
        });
    }
    await @p;
    is $r, (^200).sum, 'semaphore-protected scalar accumulation';
}

{
    my Semaphore $s .= new(1);
    my @r;
    my @p;
    for ^200 {
        my $i = $_;
        @p.push: Promise.start({
            $s.acquire;
            @r[$i]++;
            $s.release;
        });
    }
    await @p;
    is-deeply @r, [1 xx 200], 'semaphore-protected array element mutation';
}
