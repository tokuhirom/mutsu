use Test;

# `.ready` and `.kill` have to work from a thread other than the one that
# called `.start`, which is the shape roast/S17-procasync/kill.t uses:
#
#     start { await $p.ready; $p.kill }
#     await $p.start
#
# Both facts `.kill` needs -- "has it started" and "what is the pid" -- used to
# be written only into `.start`'s own copy of the instance attributes, which the
# other thread never sees: `await $p.ready` waited on a promise nobody held, and
# reaching `.kill` anyway raised X::Proc::Async::MustBeStarted against a running
# process. The ready promise is now built by the constructor, so `.ready` and
# `.start` share one promise, and `.kill` reads the pid back out of it.
#
# This was invisible while a bare `sleep` fell through the parser as a bareword
# and the child exited in 20ms on its own.

plan 4;

# `.kill` before `.start` is still an error, from any thread.
{
    my $proc = Proc::Async.new($*EXECUTABLE, '-e', 'sleep');
    throws-like { $proc.kill }, X::Proc::Async::MustBeStarted,
        '.kill before .start is X::Proc::Async::MustBeStarted';
}

# `.ready` resolves with the pid for the thread that started the process.
{
    my $proc = Proc::Async.new($*EXECUTABLE, '-e', 'sleep');
    my $run = $proc.start;
    my $pid = await $proc.ready;
    ok $pid ~~ Int && $pid > 0, '.ready resolves with the pid on the starting thread';
    $proc.kill;
    await Promise.anyof(Promise.in(15), $run);
    ok $run.status != Planned, '.kill on the starting thread ends the child';
}

# The roast shape: ready + kill on a thread that did not call `.start`. The
# child blocks forever, so it can only end if BOTH halves work cross-thread.
{
    my $proc = Proc::Async.new($*EXECUTABLE, '-e', 'sleep');
    start {
        await $proc.ready;
        $proc.kill;
    }
    my $run = $proc.start;
    await Promise.anyof(Promise.in(15), $run);
    my $ended = $run.status != Planned;
    $proc.kill unless $ended;
    ok $ended, '`start { await $p.ready; $p.kill }` ends a child blocked in `sleep`';
}
