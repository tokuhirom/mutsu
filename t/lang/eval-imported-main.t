use v6;
use Test;

# Regression from WAT--CLI: a use-ok-shaped EVAL load of an exported MAIN must
# not make that module's command-line entry point dispatch in the outer test.

plan 2;

my $exe = $*EXECUTABLE;
my $r = run(
    $exe,
    '-I', 't/lib',
    '-e', 'use Test; plan 1; use-ok("ExitMainFixture"); say "mainline"',
    :out,
    :err,
);

is $r.out.slurp(:close), "1..1\nok 1 - ExitMainFixture module can be use-d ok\nmainline\n",
    'an EVAL-loaded exported MAIN stays out of the outer dispatch';
is $r.exitcode, 0, 'the outer program keeps its successful exit status';
