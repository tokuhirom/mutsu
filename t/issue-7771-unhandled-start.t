use Test;

# A sunk `start` owns an implicit unhandled-exception handler. The diagnostic
# is emitted only when the Broken Promise is destroyed without being observed;
# retaining it, awaiting it, or inspecting its status leaves the existing
# Promise behavior in place.

plan 11;

{
    my $p = run $*EXECUTABLE, '-e',
        'start { die "boom" }; sleep 0.3; say "hello";', :out, :err;
    is $p.exitcode, 0, 'an unawaited start does not change the process exit status';
    is $p.out.slurp(:close), "hello\n", 'the mainline output is preserved';
    my $err = $p.err.slurp(:close);
    like $err, /'Unhandled exception in code scheduled on thread'/,
        'an unobserved sunk start reports its unhandled exception';
    like $err, /'boom'/, 'the diagnostic includes the exception message';

    # The worker-side exception retains its source backtrace as well.
    like $err, /'in block'/, 'the diagnostic includes the worker backtrace';
}

{
    my $p = run $*EXECUTABLE, '-e',
        'my $p = start { die "boom" }; try await $p; say "hello";', :out, :err;
    is $p.exitcode, 0, 'awaiting a broken start remains catchable';
    is $p.out.slurp(:close), "hello\n", 'the awaited case reaches the mainline';
    is $p.err.slurp(:close), '', 'awaiting a broken start does not report twice';
}

{
    my $p = run $*EXECUTABLE, '-e',
        'my $p = start { die "boom" }; sleep 0.3; say $p.status;', :out, :err;
    is $p.exitcode, 0, 'status inspection keeps the process successful';
    is $p.out.slurp(:close), "Broken\n", 'status inspection sees the Broken state';
    is $p.err.slurp(:close), '', 'status inspection suppresses the sink diagnostic';
}
