use Test;

# #17392 (ADR-0123 §7): mutsu's `await` holds its thread, so N parked `start`
# blocks need N threads. Past the process thread limit (the ecosystem
# sandbox's `ulimit -u 400`), the pool used to reject the newest task. Its
# promise broke, and Log::Dispatch's `$ready.keep; await $starter` shape hung
# because that `$ready` was never kept. Now a pool worker blocked in a wait runs
# the refused task nested. A refusal with no waiting worker to help is still
# the catchable broken promise it was.
#
# `ulimit -u` is not enforced for root and counts every thread of the user,
# so the limit is injected with `MUTSU_USER_THREAD_LIMIT`.

plan 4;

my $exe = $*EXECUTABLE.absolute;

sub run-child(Str $code, Int $limit) {
    my %e = %*ENV;
    %e<MUTSU_USER_THREAD_LIMIT> = ~$limit;
    my $p = run $exe, '-e', $code, :out, :err, :env(%e);
    my $out = $p.out.slurp(:close);
    my $err = $p.err.slurp(:close);
    ($p.exitcode, $out.trim, $err)
}

# Log::Dispatch t/040-threaded: 200 tasks each keep a promise and then park.
{
    my ($rc, $out) = run-child(q:to/CODE/, 8);
        my $starter = Promise.new;
        my (@ready, @worker);
        for ^200 -> $id {
            @ready.push: my $ready = Promise.new;
            @worker.push: start { $ready.keep; await $starter; $id }
        }
        await @ready;
        $starter.keep;
        say (await @worker).sum;
        CODE
    is-deeply ($rc, $out), (0, ~[+](^200)),
        '200 parked start blocks complete with 8 threads allowed';
}

# A chain of nested start/await, deeper than the thread limit.
{
    my ($rc, $out) = run-child(q:to/CODE/, 4);
        sub f($n) { $n == 0 ?? 0 !! await start { f($n - 1) + 1 } }
        say f(60);
        CODE
    is-deeply ($rc, $out), (0, '60'), 'a nested start/await chain 60 deep runs on 4 threads';
}

# Parked tasks that also send on a channel and await a timer.
{
    my ($rc, $out) = run-child(q:to/CODE/, 4);
        my $c = Channel.new;
        my @p = (^50).map: -> $i { start { $c.send($i); await Promise.in(0.01); $i } };
        say (await @p).sum;
        $c.close;
        say $c.list.elems;
        CODE
    is-deeply ($rc, $out), (0, "1225\n50"), 'tasks parked on timers complete past the thread limit';
}

# No pool worker is waiting, so nothing can run the task: it is still rejected.
{
    my ($rc, $out) = run-child(q:to/CODE/, 0);
        my $p = start { 42 };
        try await $p;
        say $p.status;
        say $p.cause.message.starts-with('Could not create a new Thread');
        CODE
    is-deeply ($rc, $out), (0, "Broken\nTrue"), 'a refusal with no helper still breaks the promise';
}
