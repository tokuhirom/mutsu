use Test;

# Found via LN: `-MLN` loads a module whose `sub EXPORT` mixes a parameterized
# role into the dynamic variable `$*ARGFILES`; the program then has to see the
# mixin, as it does after a `use LN;` in the program (`-M` is that `use`).
plan 2;

my $dir = $*TMPDIR.add("m-mixin-{$*PID}-{now.Int}");
$dir.mkdir;
$dir.add('MixIntoArgfiles.rakumod').spurt(q:to/MOD/);
role Marker[:$flag = True] { method marker { "mixed:$flag" } }
sub EXPORT(*@) { $*ARGFILES does Marker[:flag(False)]; Map.new }
MOD

my $proc = run $*EXECUTABLE.absolute, "-I$dir", '-MMixIntoArgfiles', '-e', 'print $*ARGFILES.marker', :out, :err;
is $proc.out.slurp(:close), 'mixed:False', 'the bare -M form keeps the mixin on $*ARGFILES';
is $proc.exitcode, 0, 'and exits cleanly';
$proc.err.slurp(:close);
