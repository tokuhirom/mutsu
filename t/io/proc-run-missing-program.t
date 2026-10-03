use Test;

plan 4;

# A missing program must fail to spawn, even when its name ends in "mutsu";
# it must not silently run the current interpreter instead.
my $p = run "/nonexistent/mutsu", "-e", "say 42", :out, :err;
is $p.out.slurp(:close), "", "missing program ending in mutsu produces no output";
is $p.exitcode, -1, "missing program ending in mutsu reports a failed spawn";

# The real interpreter path still runs.
my $q = run $*EXECUTABLE, "-e", "print 42", :out;
is $q.out.slurp(:close), "42", 'run $*EXECUTABLE still works';
is $q.exitcode, 0, 'run $*EXECUTABLE exits 0';
