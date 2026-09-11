use Test;

# Regression pin for tokuhirom/mutsu#7923: a mutable native method must publish
# what it changed before anything it wakes can observe the instance.
#
# `Proc::Async.start` keeps the `.ready` promise as soon as the child is spawned,
# so a thread blocked on `await $p.ready` is released while `start` is still
# running. It used to read `$p.started` as False there -- `started` reached the
# shared attribute cell only when the dispatcher committed `start`'s whole
# returned attribute map, which happens after the keep -- and the `.kill` that
# followed threw X::Proc::Async::MustBeStarted. That is what made roast's
# S17-procasync/kill.t test 7 fail intermittently on main.
#
# Tapping both handles is what makes this a dependable pin rather than a 3%
# coin flip: it gives `start` real work to do after the keep (a reader thread
# and supply registration per handle), so the woken thread reliably gets in
# first. Pre-fix that loses the race on 7-12 of the 12 rounds below; the fix is
# an ordering rather than a wider window, so post-fix it is 0 on any box.

plan 3;

my $rounds = 12;
my $lost-started = 0;
my $lost-pid = 0;
my $kill-failed = 0;
my $spawned = 0;

for ^$rounds {
    my $p = Proc::Async.new($*EXECUTABLE, '-e', 'sleep');
    $p.stdout.tap({ });
    $p.stderr.tap({ });
    my $ready = $p.ready;

    # Wakes inside `.start`, while the spawn is still in flight.
    my $probe = start {
        await $ready;
        ($p.started, $p.pid.defined)
    }

    my $done = $p.start;
    my ($started, $has-pid) = await $probe;
    $lost-started++ unless $started;
    $lost-pid++ unless $has-pid;

    # The failure mode the roast test actually reports.
    $kill-failed++ unless try { $p.kill: 'TERM'; True };
    $spawned++ if $p.pid.defined;
    my $proc = try await $done;
}

is $spawned, $rounds, "all $rounds children really were spawned (the race was exercised)";
is $lost-started, 0, "\$p.started is True for every thread woken by \$p.ready";
is $kill-failed + $lost-pid, 0, '$p.kill after await $p.ready never hits the not-started guard';
