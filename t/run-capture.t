use Test;

plan 10;

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
my $p4 = run("true");
isa-ok $p4, Proc, 'run without capture returns Proc';
is $p4.exitcode, 0, 'exitcode is 0 for successful command';

# Without capture options, a child inherits both of mutsu's streams.
my $exe = $*EXECUTABLE.absolute;
my $run_default = run($exe, '-e',
    'run "sh", "-c", "printf run-out; printf run-err >&2"', :out, :err);
is $run_default.out.slurp, 'run-out', 'run inherits stdout by default';
is $run_default.err.slurp, 'run-err', 'run inherits stderr by default';

my $shell_default = run($exe, '-e',
    'shell "printf shell-out; printf shell-err >&2"', :out, :err);
is $shell_default.out.slurp, 'shell-out', 'shell inherits stdout by default';
is $shell_default.err.slurp, 'shell-err', 'shell inherits stderr by default';
