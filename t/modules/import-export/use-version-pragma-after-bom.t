use Test;

# A file may start with a UTF-8 byte-order mark: the `use v6.x;` right after it
# is still the unit's first statement (HTTP::Core's modules, Math::Constants'
# tests start that way). rakudo takes it; #18872's first-statement check
# counted the BOM as a statement before it.

plan 4;

my $dir = $*TMPDIR.add("mutsu-bom-version-{$*PID}");
$dir.mkdir;
my $lib = $dir.add('lib');
$lib.mkdir;

my $script = $dir.add('bom.raku');
$script.spurt: "\x[FEFF]use v6.c;\nprint \$*RAKU.version;\n";
$lib.add('BomMod.rakumod').spurt: "\x[FEFF]use v6.d;\nunit module BomMod;\nsub bom-hi is export \{ 'hi' }\n";

my $p = run $*EXECUTABLE, $script.Str, :out, :err;
is $p.out.slurp(:close), '6.c', 'a script starting with a BOM takes its version pragma';
is $p.err.slurp(:close), '', '... without an error';

$p = run $*EXECUTABLE, '-I', $lib.Str, '-e', 'use BomMod; print bom-hi', :out, :err;
is $p.out.slurp(:close), 'hi', 'a module starting with a BOM loads';
is $p.err.slurp(:close), '', '... without an error';

END { run('rm', '-rf', $dir.Str) if $dir.e }
