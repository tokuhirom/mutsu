use Test;

# Regression pin: an exception that escapes the mainline of a program using
# `Test` must still be reported. `run()` propagated `finish()`'s error instead
# of the original one, and under `Test` a mainline exception always leaves the
# plan short — so `finish()` returned the "Test failures" plan-mismatch error and
# the real exception was silently discarded. Every such failure looked like a
# plan bug, with only `# You planned N test, but ran M` to go on. Raku prints the
# exception first and the plan diagnostic after.

plan 2;

my $script = $*TMPDIR.child("mutsu-mainline-exc-{$*PID}.raku");
$script.spurt(q:to/CODE/);
    use Test;
    plan 3;
    ok 1, 'first';
    Any.no-such-method-here;
    ok 1, 'never reached';
    CODE

my $proc = run($*EXECUTABLE, ~$script, :out, :err);
my $err = $proc.err.slurp(:close);
$proc.out.slurp(:close);
$script.unlink;

like $err, /'no-such-method-here'/,
    'the escaping exception is reported, not swallowed by the plan mismatch';
like $err, /'You planned 3 test'/,
    'the plan diagnostic is still emitted alongside it';
