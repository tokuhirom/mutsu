use Test;

# `exit` ends the process wherever it is called, and the process runs its END
# phasers on the way out -- also when the `exit` comes from a worker thread
# (#11617, ADR-11617). The main program's interpreter adopts the worker's
# `exit`: it stops its mainline wherever it is (sleeping, awaiting, receiving,
# looping), runs END, and exits with the worker's status.

plan 36;

sub run-code(Str $code) {
    my $p = run $*EXECUTABLE, '-e', $code, :out, :err;
    my $out = $p.out.slurp(:close);
    my $err = $p.err.slurp(:close);
    ($out, $err, $p.exitcode)
}

{
    my ($out, $, $code) = run-code(
        'END { say "end ran" }; start { exit 3 }; sleep 1; say "alive"');
    is $out, "end ran\n", 'exit in a start block runs END and stops the sleeping mainline';
    is $code, 3, '... with the worker\'s status';
}

{
    my ($out, $err, $code) = run-code(
        'END { say "end ran" }; start { die "dead" }; sleep 1; say "alive"');
    is $out, "end ran\n", 'a sunk start block that dies runs END';
    like $err, /'Unhandled exception in code scheduled on thread'/, '... after reporting the error';
    is $code, 1, '... and exits 1';
}

{
    my ($out, $, $code) = run-code(
        'END { say "end ran" }; await start { exit 4 }; say "alive"');
    is $out, "end ran\n", 'an await on the exiting worker\'s own promise is interrupted';
    is $code, 4, '... with the worker\'s status';
}

{
    my ($out, $, $code) = run-code(
        'END { say "end ran" }; my $c = Channel.new; start { exit 5 }; $c.receive; say "alive"');
    is $out, "end ran\n", 'a blocking Channel.receive is interrupted';
    is $code, 5, '... with the worker\'s status';
}

{
    my ($out, $, $code) = run-code(
        'END { say "end ran" }; start { exit 6 }; loop { }');
    is $out, "end ran\n", 'a busy mainline loop is interrupted';
    is $code, 6, '... with the worker\'s status';
}

{
    my ($out, $, $code) = run-code(
        'END { say "end ran" }; start { say "worker"; exit 7 }; sleep 5; say "alive"');
    is $out, "worker\nend ran\n", 'the worker\'s output comes first, once';
    is $code, 7, '... with the worker\'s status';
}

{
    my ($out, $, $code) = run-code(
        'END { say "end ran" }; start { exit 3 }; start { sleep 0.1; exit 4 }; sleep 1');
    is $out, "end ran\n", 'END runs once when two workers exit';
    is $code, 3, '... and the first exit decides the status';
}

{
    my ($out, $, $code) = run-code(
        'END { say "end ran"; exit 9 }; start { exit 3 }; sleep 1');
    is $code, 3, 'an exit in END does not override an adopted worker status';
}

# The main thread's other blocking waits give up on a pending worker `exit`
# too (#13052): a `Thread` join, a `Lock` held by the exiting worker, a
# `Semaphore`, and a condition-variable wait. Before, each one waited out the
# 1 s grace period and the worker ended the process without END.
{
    my ($out, $, $code) = run-code(
        'END { say "end ran" }; my $t = Thread.start({ exit 3 }); $t.finish; say "alive"');
    is $out, "end ran\n", 'Thread.finish on the exiting thread is interrupted';
    is $code, 3, '... with the worker\'s status';
}

{
    my ($out, $, $code) = run-code(
        'END { say "end ran" }; my $l = Lock.new; my $p = Promise.new; '
        ~ 'start { $l.lock; $p.keep; sleep 0.2; exit 5 }; await $p; '
        ~ 'try { $l.protect: { say "locked" }; CATCH { default { say "caught" } } }; say "alive"');
    is $out, "end ran\n", 'a Lock held by the exiting worker is interrupted, without running the block or CATCH';
    is $code, 5, '... with the worker\'s status';
}

{
    my ($out, $, $code) = run-code(
        'END { say "end ran" }; my $s = Semaphore.new(0); start { sleep 0.1; exit 6 }; $s.acquire; say "alive"');
    is $out, "end ran\n", 'a Semaphore.acquire is interrupted';
    is $code, 6, '... with the worker\'s status';
}

{
    my ($out, $, $code) = run-code(
        'END { say "end ran" }; my $l = Lock.new; my $cv = $l.condition; '
        ~ 'start { sleep 0.1; exit 9 }; $l.protect: { $cv.wait; say "woke" }; say "alive"');
    is $out, "end ran\n", 'a condition-variable wait is interrupted';
    is $code, 9, '... with the worker\'s status';
}

{
    my ($out, $, $code) = run-code(
        'my $l = Lock.new; my $cv = $l.condition; my $go = False; '
        ~ 'start { sleep 0.1; $l.protect: { $go = True; $cv.signal } }; '
        ~ '$l.protect: { $cv.wait until $go }; say "signalled"');
    is $out, "signalled\n", 'a condition-variable wait still wakes on its signal';
    is $code, 0, '... and the program runs on';
}

# ... and so does waiting for a child process: `run` without capture, and
# `.exitcode` of a `:out` run once its output is read (#13092).
{
    my ($out, $, $code) = run-code(
        'END { say "end ran" }; start { sleep 0.2; exit 3 }; run "sleep", "3"; say "alive"');
    is $out, "end ran\n", 'a run waiting for its child is interrupted';
    is $code, 3, '... with the worker\'s status';
}

{
    my ($out, $, $code) = run-code(
        'my $p = run "sh", "-c", "sleep 0.1; exit 7"; say $p.exitcode');
    is $out, "7\n", 'a run still reports its child\'s status';
    is $code, 0, '... and the program runs on';
}

# ... and so do blocking reads (#13100): slurping a `run :out` pipe whose
# child is still running, and a `$*IN.get` with nothing to read yet.
{
    my ($out, $, $code) = run-code(
        'END { say "end ran" }; start { sleep 0.2; exit 4 }; my $p = run "sleep", "3", :out; '
        ~ 'say $p.out.slurp; say "alive"');
    is $out, "end ran\n", 'reading a run :out pipe is interrupted';
    is $code, 4, '... with the worker\'s status';
}

{
    my $p = run $*EXECUTABLE, '-e',
        'END { say "end ran" }; start { sleep 0.2; exit 5 }; my $l = $*IN.get; say "got $l"',
        :in, :out, :err;
    my $out = $p.out.slurp(:close);
    my $ = $p.in.close;
    is $out, "end ran\n", 'a $*IN.get waiting for input is interrupted';
    is $p.exitcode, 5, '... with the worker\'s status';
}

{
    # Both lines arrive in one read and sit in the stdin buffer while the pipe
    # stays open: the second `get` must not wait on the empty fd.
    my $p = run $*EXECUTABLE, '-e',
        'start { sleep 5; exit 9 }; say $*IN.get; say $*IN.get; exit 0',
        :in, :out, :err;
    $p.in.print("a\nb\n");
    $p.in.flush;
    my $out = $p.out.slurp(:close);
    my $ = $p.in.close;
    is $out, "a\nb\n", 'a line already buffered from stdin is read without waiting';
    is $p.exitcode, 0, '... and the program runs on';
}
