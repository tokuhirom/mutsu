use Test;

plan 9;

# Raku defines `Promise.in($t)` as `$*SCHEDULER.cue({ ... }, :in($t))`, so
# replacing `$*SCHEDULER` (e.g. with a virtual-time test scheduler) redirects
# every timed promise. mutsu drives the built-in schedulers straight off its
# shared deadline heap; a *user-defined* scheduler must get the real `.cue`.

class QueueScheduler does Scheduler {
    has @.cues;
    method cue(&code, :$at, :$in, :$every, :$times = 1, :&stop, :&catch) {
        @!cues.push({ code => &code, at => $at, in => $in });
        Nil
    }
    method run-all() {
        my @todo = @!cues;
        @!cues = ();
        .<code>() for @todo;
    }
    method uncaught_handler() { Nil }
    method loads() { 0 }
}

# A class composing the Scheduler role dispatches to its own `cue`.
{
    my $s = QueueScheduler.new;
    $s.cue({ 1 }, :in(5));
    is $s.cues.elems, 1, 'a user Scheduler class dispatches to its own .cue';
    is $s.cues[0]<in>, 5, '... with the :in adverb intact';
}

# Promise.in routes through it.
{
    my $*SCHEDULER = QueueScheduler.new;
    my $p = Promise.in(0.001);
    is $*SCHEDULER.cues.elems, 1, 'Promise.in cues on the user $*SCHEDULER';
    is $*SCHEDULER.cues[0]<in>, 0.001, '... passing the delay as :in';
    sleep 0.05;
    is $p.status, Planned, 'the promise is NOT kept by real time';
    $*SCHEDULER.run-all;
    is $p.status, Kept, 'running the cued code keeps the promise';
    is await($p), True, 'and the promise is kept with True';
}

# Promise.at routes through it too. Rakudo cues it as `:in($at - now)`, so the
# scheduler measures the delay against its own clock.
{
    my $*SCHEDULER = QueueScheduler.new;
    my $p = Promise.at(now + 30);
    ok 29 < $*SCHEDULER.cues[0]<in> <= 30, 'Promise.at cues with :in($at - now)';
    sleep 0.05;
    is $p.status, Planned, 'Promise.at is not kept by real time either';
}

# vim: expandtab shiftwidth=4
