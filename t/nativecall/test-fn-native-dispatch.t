use Test;

# Regression guard for the VM-side Test-function dispatch
# (src/vm/vm_native_test.rs). The VM routes Test functions straight to their
# typed handler instead of through `call_function`. A name that is BOTH a Test
# function and a core builtin (notably `run`, the Proc spawner, vs
# `Test::Util::run`) must NOT be hijacked: the builtin has to keep working
# inside a test file.

plan 3;

# `run` here is the builtin Proc spawner, not Test::Util::run.
my $proc = run($*EXECUTABLE, '-e', 'print "hello-from-run"', :out);
my $out = $proc.out.slurp;
is $out, "hello-from-run", "builtin run is not hijacked by Test::Util::run";

# Ordinary Test functions keep working alongside it.
ok True, "ok still works after a builtin run call";
is 2 + 2, 4, "is still works after a builtin run call";
