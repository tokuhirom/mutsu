use Test;
use nqp;

# The Threads family of nqp:: ops (#11502) work on the same Thread object as
# `Thread.new` / `.run` / `.finish` / `.id`. Expected answers are Rakudo
# 2026.09's.

plan 14;

my $cur := nqp::currentthread();
is nqp::threadid($cur), $*THREAD.id, 'threadid of currentthread is $*THREAD.id';
is nqp::threadlockcount($cur), 0, 'no locks held at the start';

{
    my $x = 0;
    my $t := nqp::newthread(nqp::getattr(-> { $x = 42 }, Code, '$!do'), 0);
    ok nqp::threadid($t) != nqp::threadid($cur), 'newthread allocates its own id';
    is $x, 0, 'newthread does not start the thread';
    ok nqp::eqaddr(nqp::threadrun($t), $t), 'threadrun answers the handle';
    ok nqp::eqaddr(nqp::threadjoin($t), $t), 'threadjoin answers the handle';
    is $x, 42, 'the thread ran, and threadjoin waited for it';
}

ok nqp::isnull(nqp::threadyield()), 'threadyield answers null';

{
    my $l = Lock.new;
    my $m = Lock.new;
    my @seen;
    $l.protect: {
        $l.protect: { @seen.push: nqp::threadlockcount(nqp::currentthread()) };
        $m.protect: { @seen.push: nqp::threadlockcount(nqp::currentthread()) };
    };
    is-deeply @seen, [1, 2], 'a re-entered lock counts once; two locks count two';
    is nqp::threadlockcount(nqp::currentthread()), 0, 'released locks are uncounted';

    my $taken = Promise.new;
    my $done = Promise.new;
    my $t := nqp::newthread(
        nqp::getattr(-> { $l.lock; $taken.keep; await $done; $l.unlock }, Code, '$!do'), 0);
    nqp::threadrun($t);
    await $taken;
    is nqp::threadlockcount($t), 1, 'threadlockcount reads another thread';
    $done.keep;
    nqp::threadjoin($t);
    is nqp::threadlockcount($t), 0, 'and sees it release the lock';
}

throws-like { nqp::threadid(42) }, Exception,
    message => /'must have representation MVMThread'/,
    'a non-thread handle dies';
throws-like { nqp::newthread(42, 0) }, Exception,
    message => /'must be a code handle'/,
    'newthread wants code';
