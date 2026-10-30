use Test;

plan 5;

# `-M'Module arg,list'` is `use Module arg,list;` (static's bin/static-check.raku
# runs `-Mstatic::CLI "fail-on-mismatch","1","1"`), and a bare `-MModule` still
# loads the module.
my $dir = $*TMPDIR.add("m-option-{$*PID}-{now.Int}");
$dir.mkdir;
my $out-file = $dir.add('out.txt');
$dir.add('Recorder.rakumod').spurt(qq:to/MOD/);
sub EXPORT(\$a = 'none', \$b = 'none') \{ q|{$out-file}|.IO.spurt("\$a|\$b"); Map.new }
MOD

sub run-with(*@switches) {
    $out-file.unlink if $out-file.e;
    my $proc = run $*EXECUTABLE.absolute, "-I$dir", |@switches, '-e', 'print "ran"', :out, :err;
    my $out = $proc.out.slurp(:close);
    $proc.err.slurp(:close);
    ($proc.exitcode, $out, $out-file.e ?? $out-file.slurp !! 'not loaded')
}

my ($status, $out, $recorded) = run-with('-MRecorder "x","y"');
is $status, 0, 'a module with an argument list loads';
is $recorded, 'x|y', 'and its EXPORT receives the arguments';
is $out, 'ran', 'the program still runs';

($status, $out, $recorded) = run-with('-M', 'Recorder "z"');
is $recorded, 'z|none', 'the separate -M form takes arguments too';

($status, $out, $recorded) = run-with('-MRecorder');
is $recorded, 'none|none', 'a bare module name still loads';

sub remove-tree(IO::Path $path) {
    if $path.d { remove-tree($_) for $path.dir; $path.rmdir } else { $path.unlink }
}
END { remove-tree($dir) }
