use v6;
use Test;

# ADR-0105 S0 ("pin the oracle"): a logging user Scheduler records every
# `.cue` dispatch. Rakudo (per ADR-0105 Appendix A, measured against raku
# 2026.07) routes an awaited promise's wake-up, a `.then` callback, a bound
# `Promise.new(scheduler => ...)`, and a `start` block under a user
# `$*SCHEDULER` all through that scheduler's own `.cue`; mutsu's blocking
# `await` and pool-direct dispatch never call it. Every `todo`'d assertion
# below is expected to start passing, in order, as ADR-0105's slices S1-S4
# land — see docs/adr/0105-promise-resolution-dispatches-through-the-promise-scheduler.md
# §8. Do not remove a `todo` here without the matching slice actually
# landing; do not add a fix to make one pass without updating that plan.

plan 6;

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

# F4a: Promise.scheduler exposes the scheduler it was constructed with.
{
    my $s = LogSched.new;
    my $p = Promise.new(scheduler => $s);
    my $sched = try { $p.scheduler };
    todo 'ADR-0105 S1 (D1): Promise.scheduler does not exist yet';
    is $sched, $s, 'Promise.scheduler returns the bound scheduler (F4)';
}

# F1a: keeping an awaited promise cues the wake-up through its scheduler.
{
    my $s = LogSched.new;
    my $p = Promise.new(scheduler => $s);
    my $started = Promise.new;
    my $t = start {
        $started.keep;
        await $p;
    };
    await $started;
    sleep 0.05;
    $p.keep(1);
    await $t;
    todo 'ADR-0105 S1/S2 (D1/D2): await-wake does not dispatch through the bound scheduler yet';
    ok $s.log.elems >= 1, 'keep cues the awaiter wake-up through the promise scheduler (F1)';
}

# F1b: a .then callback is dispatched through the promise's scheduler too.
{
    my $s = LogSched.new;
    my $p = Promise.new(scheduler => $s);
    my $done = Promise.new;
    $p.then({ $done.keep(1) });
    $p.keep(1);
    await $done;
    todo 'ADR-0105 S1/S2 (D1/D2): .then dispatch does not go through the bound scheduler yet';
    ok $s.log.elems >= 1, '.then callback dispatch cues through the promise scheduler (F1)';
}

# F4b: `start` under a user $*SCHEDULER cues its task through it.
{
    my $s = LogSched.new;
    my $t = do {
        my $*SCHEDULER = $s;
        start { 42 };
    }
    is await($t), 42, 'the start block still completes normally';
    todo 'ADR-0105 S1 (D1): start does not consult a user $*SCHEDULER yet';
    ok $s.log.elems >= 1, 'start cues its task through the user $*SCHEDULER (F4)';
}

# Experiment 3: a keeper's own continuation (setting $flag before parking on
# `sleep`) is observed by the thread it woke, before that thread's dispatch
# has a chance to overtake it. Rakudo: 30/30. mutsu (ADR-0105 Appendix A):
# 28/30 — real but rare, so this assertion may occasionally pass today by
# chance; that is harmless under `todo`.
{
    my $init = Promise.new;
    my $flag = 0;
    my $p = start { $init.keep; $flag = 1; sleep 0.1 };
    await $init;
    my $observed = $flag;
    await $p;
    todo 'ADR-0105 S4 (D4): a worker-submitted task may still overtake its still-running submitter';
    is $observed, 1, 'the awaiter observes the keeper continuation before dispatch overtakes it (experiment 3)';
}

# vim: expandtab shiftwidth=4
