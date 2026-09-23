use Test;

# as-cli-arguments uses `run ... :merge` to capture the child script's output.
# Rakudo puts the merged stream in .out; mutsu used to inherit stdout and
# return Nil from .out instead.
plan 2;

my $proc = run $*EXECUTABLE, '-e',
    q[print "stdout"; $*ERR.print("stderr")], :merge;

is $proc.out.slurp(:close), 'stdoutstderr',
    ':merge captures stdout and stderr together in .out';
nok $proc.err.defined, ':merge leaves .err undefined';
