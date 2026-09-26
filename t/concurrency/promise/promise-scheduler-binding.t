use v6;
use Test;

# ADR-0105 D1: a promise carries the scheduler it was constructed under, at
# every site Rakudo binds it, and a `start` body under a user `$*SCHEDULER`
# is cued through that scheduler's `.cue`.

plan 16;

class LogSched does Scheduler {
    has $.wrapped = $*SCHEDULER;
    has @.log;
    method cue(&code, *%opts) {
        @!log.push(%opts.keys.sort.join(','));
        $!wrapped.cue(&code, |%opts);
    }
    method uncaught_handler is rw { $!wrapped.uncaught_handler }
    method loads() { $!wrapped.loads }
}

# A scheduler that only records what it was cued with, and runs it on demand
# (honouring `:catch`, as a real scheduler does).
class HoldSched does Scheduler {
    has @.held;
    method cue(&code, :&catch, *%opts) {
        @!held.push(&catch ?? { code(); CATCH { default { catch($_) } } } !! &code);
        Nil
    }
    method run-all() { .() for @!held.splice }
    method uncaught_handler is rw { my $ }
    method loads() { 0 }
}

{
    my $s = LogSched.new;
    is Promise.new(scheduler => $s).scheduler, $s,
        'Promise.new(:scheduler) is read back by .scheduler';
    is Promise.new(scheduler => 42).scheduler, 42,
        'an explicit non-scheduler value is kept as given';
    isa-ok Promise.new.scheduler, Scheduler,
        'an unbound promise reports the built-in scheduler';
    my $p = do { my $*SCHEDULER = $s; Promise.new };
    is $p.scheduler, $s, 'Promise.new binds a user $*SCHEDULER';
}

{
    my $s = LogSched.new;
    my $p = Promise.in(1000, :scheduler($s));
    is $s.log.join('|'), 'in', 'Promise.in(:scheduler) cues through the given scheduler';
    is $p.scheduler, $s, 'Promise.in binds its :scheduler';
}

{
    my $user = LogSched.new;
    my $builtin = $*SCHEDULER;
    my $p = do {
        my $*SCHEDULER = $user;
        Promise.in(0.01, :scheduler($builtin));
    }
    await $p;
    is $user.log.elems, 0, 'an explicit built-in :scheduler overrides a user $*SCHEDULER';
}

{
    my $s = LogSched.new;
    my $p = Promise.new(scheduler => $s);
    my $then = $p.then({ 1 });
    is $then.scheduler, $s, 'a .then result inherits the scheduler';
    $p.keep(1);
    await $then;
}

{
    my $s = HoldSched.new;
    my $ran = 0;
    my $p = do {
        my $*SCHEDULER = $s;
        start { $ran = 1; 42 };
    }
    is $s.held.elems, 1, 'start under a user $*SCHEDULER cues its body through it';
    is $p.status, Planned, 'the body does not run until the scheduler runs it';
    is $p.scheduler, $s, 'the start promise is bound to that scheduler';
    $s.run-all;
    is await($p), 42, 'running the cued body keeps the promise with its value';
}

{
    my $s = HoldSched.new;
    my $p = do {
        my $*SCHEDULER = $s;
        start { die 'boom' };
    }
    # The cued body runs inline in `run-all`, so the promise is settled
    # afterwards. (Awaiting it would park on a wake-up cued through the
    # holding scheduler, which never runs it again.)
    $s.run-all;
    is $p.status, Broken, 'a dying cued start body breaks the promise';
    like $p.cause.message, /boom/, '... with the exception as its cause';
}

{
    my $s = HoldSched.new;
    my $p = Promise.start({ 7 }, :scheduler($s));
    $s.run-all;
    is await($p), 7, 'Promise.start(:scheduler) cues through the given scheduler';
}

{
    my $user = HoldSched.new;
    my $builtin = $*SCHEDULER;
    my $p = do {
        my $*SCHEDULER = $user;
        Promise.start(:scheduler($builtin), { 9 });
    }
    is await($p), 9, 'Promise.start(:scheduler(builtin)) escapes a user $*SCHEDULER';
}

# vim: expandtab shiftwidth=4
