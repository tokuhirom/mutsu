use Test;

# `.ready` is the handshake that lets another thread act on a Proc::Async the
# moment the process exists. Both halves of it used to be carried through the
# instance's attribute map, which a native mutable method observes as a
# *snapshot*: the method is handed a copy and the caller commits the result only
# after it returns. Two threads calling methods on one Proc::Async therefore
# raced.
#
#   * `.ready` racing `.start` minted a fresh promise out of a pre-spawn
#     snapshot and handed it to `.start` through the map. `.start` was already
#     past its own resolve point holding an older snapshot, so nothing ever kept
#     that promise: `await $p.ready` hung forever.
#   * a thread woken by `.ready` reached `.kill` while the instance still showed
#     the pre-spawn map, so `.kill` threw X::Proc::Async::MustBeStarted.
#
# The promise is built by the constructor now and kept with the pid by `.start`.
# A promise is shared by reference rather than copied with the map, so every
# snapshot — however stale — reaches the same one.

plan 6;

# A deadline, so a regression fails an assertion instead of hanging the file.
sub settle($promise) {
    await Promise.anyof($promise, Promise.in(20));
}

# 1. The racing shape, with a child that needs no killing so nothing here
# depends on signal timing. Repeated, because whether `.ready` lands before or
# after `.start` has committed its attributes is a genuine race: one round
# reproduced the old hang about three times in four, so a handful of rounds
# makes a regression all but certain to be caught.
{
    my $resolved = 0;
    for ^5 {
        my $p = Proc::Async.new: $*EXECUTABLE, '-e', 'exit 0';
        my $observer = start { await $p.ready; 'ready' };
        await $p.start;
        settle($observer);
        $resolved++ if $observer.status ~~ Kept;
    }
    is $resolved, 5, '.ready resolves for a thread racing .start';
}

# 2-3. The same race, but the woken thread goes on to use the handle: this is
# the shape roast/S17-procasync/kill.t exercises.
{
    my $p = Proc::Async.new: $*EXECUTABLE, '-e', 'sleep 20';
    my $killer = start {
        await $p.ready;
        $p.kill: SIGTERM;
        'killed'
    };
    my $proc = await $p.start;
    settle($killer);
    is $killer.result, 'killed', '.kill after .ready does not throw MustBeStarted';
    is $proc.signal, 15, 'the signal actually reached the child';
}

# 4-5. The ordinary shape still works: `.ready` fetched before `.start`.
{
    my $p = Proc::Async.new: $*EXECUTABLE, '-e', 'sleep 20';
    my $ready = $p.ready;
    my $started = $p.start;
    settle($ready);
    is $ready.status, Kept, '.ready fetched before .start resolves';
    ok $ready.result > 0, '.ready resolves with the pid';
    $p.kill: SIGTERM;
    # Bound, not sunk: a Proc that died of a signal throws when it is sunk.
    my $reaped = await $started;
}

# 6. The fix must not make every Proc::Async look started: an un-started one
# still refuses `.kill`.
{
    my $p = Proc::Async.new: $*EXECUTABLE, '-e', 'exit 0';
    throws-like { $p.kill }, X::Proc::Async::MustBeStarted, :method<kill>,
        '.kill before .start still throws MustBeStarted';
}
