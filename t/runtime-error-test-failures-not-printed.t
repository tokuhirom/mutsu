use Test;

plan 4;

# A failing test file used to print an extra "Runtime error: Test failures"
# line to stderr that rakudo does not produce: `run()` returned the failure
# as a RuntimeError and `main` rendered it as an uncaught error, instead of
# treating "some assertions failed" (or a planned/ran mismatch) as an
# ordinary non-zero exit -- the same way the bail-out branch already does.
# The exit status was already correct either way.

sub run_snippet(Str:D $code) {
    my $proc = run $*EXECUTABLE.absolute, '-e', $code, :out, :err;
    my $out = $proc.out.slurp(:close);
    my $err = $proc.err.slurp(:close);
    ($proc.exitcode, $out, $err);
}

my ($status, $out, $err) = run_snippet 'use Test; plan 2; ok 1, "a"; ok 0, "b";';
is $status, 1, 'a failing assertion exits 1';
unlike $err, /'Runtime error'/, '...with no extra "Runtime error" line on stderr';

($status, $out, $err) = run_snippet 'use Test; plan 3; ok 1, "a";';
is $status, 255, 'a short plan exits 255';
unlike $err, /'Runtime error'/, '...with no extra "Runtime error" line on stderr either';
