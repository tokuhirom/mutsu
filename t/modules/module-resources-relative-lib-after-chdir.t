use Test;
use lib 'roast/packages/Test-Helpers';
use Test::Util;

plan 3;

# `%?RESOURCES` names the distribution's own `resources/` directory, fixed when
# the module loads: a relative `-I lib` must not make it follow a later `chdir`
# (App::RaCoCo's reporter tests run `indir` into a scratch directory, #13126).
my $dist = make-temp-dir;
$dist.add('lib').mkdir;
$dist.add('resources').mkdir;
$dist.add('resources/x.txt').spurt("hello\n");
$dist.add('META6.json').spurt(
    '{"name":"RMod","version":"0.1","provides":{"RMod":"lib/RMod.rakumod"},"resources":["x.txt"]}');
$dist.add('lib/RMod.rakumod').spurt(
    'unit module RMod;' ~ "\n" ~ 'our sub res() { %?RESOURCES<x.txt>.slurp }' ~ "\n");
$dist.add('main.raku').spurt(
    "use RMod;\nprint RMod::res();\nindir(\$*TMPDIR, \{ print RMod::res() \});\n");

my $proc = run $*EXECUTABLE, '-I', 'lib', 'main.raku', :cwd($dist), :out, :err;
is $proc.out.slurp(:close), "hello\nhello\n", 'resource read before and inside indir';
is $proc.exitcode, 0, 'exits cleanly';
is $proc.err.slurp(:close), '', 'no diagnostics';
