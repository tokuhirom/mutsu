use Test;
use nqp;

# `nqp::signal` (#11502, ADR-11502 §2.4): the Asynchronous op that pushes
# `[schedulee, signum]` onto a `ConcBlockingQueue` for each delivered signal.
# Every expectation matches rakudo 2026.09.

plan 16;

my class Q is repr('ConcBlockingQueue') { }
my class TC is repr('AsyncTask') { }

# The next item on `q`, or 'timeout' if none arrives within `$ms`: a timer
# on the same queue bounds every wait, so a lost signal fails, never hangs.
sub next-item(\q, $ms = 5000) {
    my $t := nqp::timer(q, 'timeout', $ms, 0, TC);
    my $item := nqp::shift(q);
    nqp::cancel($t);
    $item
}

{
    my $q := nqp::create(Q);
    my $sq := nqp::create(Q);
    my $setup := -> { };
    my $handler := -> $signum { };
    my $h := nqp::signal($sq, $setup, $q, $handler, 12, TC);
    is $h.^name, 'TC', 'the op answers a handle of the given type';
    is $h.REPR, 'AsyncTask', 'of REPR AsyncTask';

    my $s := nqp::shift($sq);
    is nqp::elems($s), 1, 'the setup notification is a one-item array';
    ok nqp::atpos($s, 0) === $setup, 'holding the setup schedulee';

    run 'kill', '-USR2', ~$*PID;
    my $item := next-item($q);
    is nqp::elems($item), 2, 'a delivered signal pushes a two-item array';
    ok nqp::atpos($item, 0) === $handler, 'the schedulee first';
    is nqp::atpos($item, 1).^name, 'BOOTInt', 'then the signal number, boxed';
    is nqp::unbox_i(nqp::atpos($item, 1)), 12, 'which is the signal delivered';

    # A second handler on the same signal: each one gets its own push, and a
    # cancelled one gets none while the other keeps the signal handled.
    my $h2 := nqp::signal($sq, $setup, $q, $handler, 12, TC);
    nqp::shift($sq);
    ok nqp::cancel($h) === $h, 'cancel answers the handle';
    run 'kill', '-USR2', ~$*PID;
    ok nqp::atpos(next-item($q), 0) === $handler, 'the live handler still pushes';
    is next-item($q, 300), 'timeout', 'the cancelled one does not';
    nqp::cancel($h2);
}

{
    # Cancelling the last handler gives the signal back to its default
    # disposition: the process dies of it, as with no handler at all.
    my $code = q:to/CODE/;
        use nqp;
        my class Q is repr('ConcBlockingQueue') { }
        my class TC is repr('AsyncTask') { }
        my $q := nqp::create(Q);
        my $h := nqp::signal($q, -> { }, $q, -> $n { }, 10, TC);
        nqp::shift($q);
        nqp::cancel($h);
        run 'kill', '-USR1', ~$*PID;
        sleep 5;
        CODE
    my $proc = run $*EXECUTABLE, '-e', $code, :out, :err;
    $proc.out.slurp(:close);
    $proc.err.slurp(:close);
    is $proc.signal, 10, 'a cancelled last handler restores the default disposition';
}

{
    my $q := nqp::create(Q);
    throws-like { nqp::signal($q, -> { }, $q, -> $n { }, 12, Q) }, Exception,
        message => 'signal result type must have REPR AsyncTask',
        'the handle type must be an AsyncTask';
    throws-like { nqp::signal($q, -> { }, TC, -> $n { }, 12, TC) }, Exception,
        message => 'signal target queue must have ConcBlockingQueue REPR',
        'the target must be a queue';
    throws-like { nqp::signal(TC, -> { }, $q, -> $n { }, 12, TC) }, Exception,
        message => 'signal setup notify queue must have ConcBlockingQueue REPR',
        'so must the setup notify queue';
    throws-like { nqp::signal($q, -> { }, $q, -> $n { }, 0, TC) }, Exception,
        message => 'Unsupported signal handler 0',
        'signal 0 is refused';
}
