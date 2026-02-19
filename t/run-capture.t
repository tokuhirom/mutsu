use Test;

plan 6;

# run with :err captures stderr
my $p1 = run("echo", "hello", :err);
isa-ok $p1, Proc, 'run returns a Proc';
is $p1.err.slurp, '', 'stderr is empty for echo';

# run with :out captures stdout
my $p2 = run("echo", "hello", :out);
is $p2.out.slurp, "hello\n", 'stdout captured with :out';

# run with :err captures stderr content
my $p3 = run("sh", "-c", "echo errmsg >&2", :err);
is $p3.err.slurp, "errmsg\n", 'stderr content captured';

# run without :err/:out still works
my $p4 = run("echo", "test");
isa-ok $p4, Proc, 'run without capture returns Proc';
is $p4.exitcode, 0, 'exitcode is 0 for successful command';
