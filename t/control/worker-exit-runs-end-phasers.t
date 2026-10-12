use Test;

# `exit` ends the process wherever it is called, and the process runs its END
# phasers on the way out -- also when the `exit` comes from a worker thread
# (#11617, ADR-11617). The main program's interpreter adopts the worker's
# `exit`: it stops its mainline wherever it is (sleeping, awaiting, receiving,
# looping), runs END, and exits with the worker's status.

plan 16;

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
