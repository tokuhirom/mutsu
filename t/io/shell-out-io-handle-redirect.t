use Test;

plan 5;

# `shell(..., :out($handle))` redirects the child's stdout into the handle's
# file, as `run` does (App::RaCoCo's RunProc writes a command's output into a
# file this way).
my $f = $*TMPDIR.add("mutsu-shell-out-handle-{$*PID}.txt");
LEAVE $f.unlink if $f.e;

my $out = $f.open(:w);
my $p = shell('echo hello', :$out);
$out.close;
is $f.slurp, "hello\n", 'stdout of the command lands in the file';
is $p.exitcode, 0, 'exitcode 0';

$out = $f.open(:w);
shell('V1=v1 V2=v2 sh -c \'echo "vars: $V1 $V2"\'', :out($out));
$out.close;
is $f.slurp.trim, 'vars: v1 v2', 'with env assignments in the command';

# stderr still goes where it did; with :merge it follows stdout.
$out = $f.open(:w);
shell('echo to-err >&2; echo to-out', :$out, :merge);
$out.close;
is $f.slurp.lines.sort.List, ('to-err', 'to-out'), ':merge sends stderr to the same file';

# Without a handle the capture form is unchanged.
is shell('echo captured', :out).out.slurp(:close), "captured\n", ':out still captures';
