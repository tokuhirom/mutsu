use Test;

plan 8;

# IO::Pipe is an IO::Handle: the routine `slurp` reads the stream it is given
# instead of opening its stringification as a path (App::RaCoCo's
# `slurp($proc.err)` in the precompiler's error handler).
my $p = shell('echo oops >&2; exit 1', :err);
ok $p.err ~~ IO::Handle, 'a pipe is an IO::Handle';
is $p.err.^name, 'IO::Pipe', 'and keeps its own class';
is slurp($p.err), "oops\n", 'slurp($proc.err)';

my $q = shell('echo out', :out);
is slurp($q.out), "out\n", 'slurp($proc.out)';

my $r = shell('echo closed', :out);
is slurp($r.out, :close), "closed\n", 'slurp with :close';

my $f = $*TMPDIR.add("mutsu-slurp-pipe-{$*PID}.txt");
LEAVE $f.unlink if $f.e;
$f.spurt("abc\n");
my $h = $f.open;
is slurp($h), "abc\n", 'an ordinary file handle still works';
$h.close;

is slurp($f), "abc\n", 'a path still works';
is slurp($f.Str), "abc\n", 'a path string still works';
