use Test;

# A module the parse already loaded registered its END phasers, and rakudo
# runs them when the compile fails afterwards: the SORRY report first, then
# END (#18918), under `-c` too.

plan 4;

sub run-program(*@args) {
    my $p = run $*EXECUTABLE, '-I', 't/lib', |@args, :out, :err;
    my $out = $p.out.slurp(:close);
    my $err = $p.err.slurp(:close);
    ($out, $err, $p.exitcode)
}

{
    my ($out, $err, $status) = run-program('-e', 'use EndOrderLate; 1 +;');
    is $out, "EndOrderLate END\n", 'the loaded module runs its END';
    like $err, /'Missing required term after infix'/, '... after the compile error is reported';
    is $status, 1, '... and the status is the compile failure';
}

{
    my ($out, $err, $status) = run-program('-c', '-e', 'use EndOrderLate; 1 +;');
    is $out, "EndOrderLate END\n", 'so does a failed -c compile';
}
