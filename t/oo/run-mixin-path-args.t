use v6;
use Test;

# `run` stringifies an object argument with `.Str`: an IO::Path with a role
# mixed in (Temp::Path's `make-temp-path` does `$p does AutoDel[$p]`) names
# its file, not its type. Same as raku.

plan 4;

my $dir = $*TMPDIR.add("mutsu-run-mixin-{$*PID}");
$dir.mkdir;
my $file = $dir.add('data.txt');
$file.spurt: "a\nb\n";

role Plain { }
role Param[$x] { }

my $plain = $file.Str.IO;
$plain does Plain;
my $p = run $*EXECUTABLE, '-e', 'print slurp @*ARGS[0]', $plain, :out;
is $p.out.slurp(:close), "a\nb\n", 'a path with a role mixed in';

my $param = $file.Str.IO;
$param does Param[$param];
$p = run $*EXECUTABLE, '-e', 'print slurp @*ARGS[0]', $param, :out;
is $p.out.slurp(:close), "a\nb\n", 'a path with a parameterized role mixed in';

is $param.raku, qq|IO::Path+\{Param[IO::Path]\}.new("{$file.Str}", :SPEC(IO::Spec::Unix), :CWD("{$*CWD}"))|,
    '.raku names the mixin type';

class Named { method Str { $file.Str } }
$p = run $*EXECUTABLE, '-e', 'print slurp @*ARGS[0]', Named.new, :out;
is $p.out.slurp(:close), "a\nb\n", 'an object of a user class';

$file.unlink;
$dir.rmdir;
