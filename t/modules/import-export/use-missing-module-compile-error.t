use Test;

# Every `use` loads at compile time, so a module nobody can find is a compile
# error of the unit that names it: `===SORRY!=== Error while compiling`, at
# the `use`'s line, before any of the unit's run-time code. Inside a module
# the error names the module's file, not the importer's.

plan 7;

my $dir = $*TMPDIR.add("use-missing-{$*PID}-{(^1_000_000).pick}");
$dir.mkdir;

my $script = $dir.add('main.raku');
$script.spurt("say 'ran';\n\nuse NoSuchModuleAnywhere;\n");
my $p = run $*EXECUTABLE, $script, :out, :err;
my $out = $p.out.slurp(:close);
my $err = $p.err.slurp(:close);
is $out, '', 'no run-time code ran';
like $err, /^ '===SORRY!=== Error while compiling ' .* 'main.raku' \n/, 'a compile error';
like $err, /'Could not find NoSuchModuleAnywhere'/, 'naming the module';
like $err, /'main.raku:3' \n/, 'at the line of the use';

$dir.add('Holder.rakumod').spurt("unit module Holder;\n\nuse NoSuchModuleAnywhere;\n");
$p = run $*EXECUTABLE, '-I', $dir.absolute, '-e', 'use Holder;', :err;
$err = $p.err.slurp(:close);
like $err, /^ '===SORRY!=== Error while compiling ' .* 'Holder.rakumod'/, 'in a module: its file';
like $err, /'Holder.rakumod' \N* ':3' \n/, '...and its line';

try EVAL 'use NoSuchModuleAnywhere';
isa-ok $!, X::CompUnit::UnsatisfiedDependency, 'EVAL still throws the typed exception';

$dir.add('Holder.rakumod').unlink;
$script.unlink;
$dir.rmdir;
