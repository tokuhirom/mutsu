use Test;
use nqp;

# The scheduler-queue part of the Asynchronous nqp ops (#11502, ADR-11502):
# `is repr('ConcBlockingQueue')` / `is repr('AsyncTask')` classes,
# `nqp::timer` and `nqp::cancel`. Every expectation matches rakudo 2026.09.

plan 18;

my class Q is repr('ConcBlockingQueue') { }
my class TC is repr('AsyncTask') { }

{
    my $q := nqp::create(Q);
    is $q.^name, 'Q', 'a queue instance keeps its class';
    is Q.REPR, 'ConcBlockingQueue', 'the type reports its REPR';
    is $q.REPR, 'ConcBlockingQueue', 'so does the instance';
    nqp::push($q, 1);
    nqp::push($q, 2);
    is nqp::elems($q), 2, 'elems counts the waiting items';
    is nqp::shift($q), 1, 'shift takes the oldest item';
    is nqp::elems($q), 1, 'and removes it';
    throws-like { nqp::pop($q) }, Exception,
        message => 'This representation (ConcBlockingQueue) does not support positional access (for type Q)',
        'positional access is refused';
}

{
    my $q := nqp::create(Q);
    start { sleep 0.05; nqp::push($q, 'late') };
    is nqp::shift($q), 'late', 'shift blocks until another thread pushes';
}

{
    is nqp::create(TC).^name, 'TC', 'a task instance keeps its class';
    is TC.REPR, 'AsyncTask', 'the task type reports its REPR';
    my $q := nqp::create(Q);
    my $h := nqp::timer($q, 'x', 10, 0, TC);
    is $h.^name, 'TC', 'timer answers a handle of the given type';
    is $h.REPR, 'AsyncTask', 'whose REPR is AsyncTask';
    is nqp::shift($q), 'x', 'the timer pushes its schedulee onto the queue';
    nqp::timer($q, { 42 }, 10, 0, TC);
    is nqp::shift($q)(), 42, 'the schedulee arrives as itself';
}

{
    my $q := nqp::create(Q);
    my $h := nqp::timer($q, 't', 0, 20, TC);
    is (^3).map({ nqp::shift($q) }).join, 'ttt', 'a repeating timer fires again';
    is nqp::cancel($h).^name, 'TC', 'cancel answers the handle';
    my $once := nqp::timer($q, 'never', 100, 0, TC);
    nqp::cancel($once);
    sleep 0.3;
    ok nqp::elems($q) <= 1, 'a cancelled timer stops pushing';
}

throws-like { nqp::timer(nqp::list(), 'x', 10, 0, TC) }, Exception,
    message => 'timer target queue must have ConcBlockingQueue REPR',
    'timer refuses another queue';
