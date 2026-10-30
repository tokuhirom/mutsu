use Test;

# `run :err($handle)` writes the child's stderr to that handle's file and
# `run :in($handle)` feeds the child's stdin from it, as rakudo does
# (static's t/14-cli.rakutest wires a child's streams to files this way).

plan 5;

my $dir = $*TMPDIR.add("run-err-in-handle-{$*PID}");
$dir.mkdir;
LEAVE { .unlink for $dir.dir; $dir.rmdir }

my $out = $dir.add('stdout');
my $err = $dir.add('stderr');
my $o = $out.open(:w);
my $e = $err.open(:w);
my $p = run $*EXECUTABLE, '-e', 'note "CHILD-ERR"; say "CHILD-OUT"', :out($o), :err($e);
.close for $o, $e;
is $p.exitcode, 0, 'the child ran';
is $out.slurp, "CHILD-OUT\n", ':out($handle) still captures stdout';
is $err.slurp, "CHILD-ERR\n", ':err($handle) captures stderr';

# stderr is inherited by a grandchild started without its own :err
$o = $out.open(:w);
$e = $err.open(:w);
run $*EXECUTABLE, '-e', 'run $*EXECUTABLE, "-e", q|note "GRANDCHILD-ERR"|', :out($o), :err($e);
.close for $o, $e;
is $err.slurp, "GRANDCHILD-ERR\n", 'a grandchild inherits the redirected stderr';

my $input = $dir.add('stdin');
$input.spurt("line from file\n");
my $i = $input.open(:r);
$o = $out.open(:w);
run $*EXECUTABLE, '-e', 'print "got: " ~ $*IN.get', :in($i), :out($o);
.close for $i, $o;
is $out.slurp, 'got: line from file', ':in($handle) feeds the child stdin';
