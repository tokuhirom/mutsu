use v6;
use lib 't/lib';
use Test;

# #7836: an END phaser runs at program exit, when `current_unit` is back to the
# main compunit -- but a package-qualified name in its body is in scope by the
# rules of the compunit that DECLARED it (#7797's `qualified_name_visible_here`,
# whose strongest rule is "a compunit always sees a package it declares
# itself"). `EndPhaser` already recorded and restored the declaring *package*
# for exactly this reason; the compunit needed the same treatment.
#
# Only observable when the module's visibility never reached the main compunit:
# one `use`d from inside an `EVAL`, whose grant went to the ephemeral EVAL unit.
# `Test`'s own `use-ok` is literally `EVAL "use $module"`, and `Log::Async`'s
# `END { Log::Async.instance.done ... }` reached through it took the whole exit
# sequence down -- the script's own END phasers never ran either.
#
# The assertion is the child process's full output: that the module's END
# produced its value AND that a later-declared END still ran after it.

plan 2;

my $prog = q:to/CODE/;
use MONKEY-SEE-NO-EVAL;
use lib 't/lib';
EVAL 'use EndUnitQualified';
say 'mainline done';
END { say 'script END ran' }
CODE

my $script = 'tmp/end-phaser-declaring-compunit-child.raku';
$script.IO.spurt($prog);

my $proc = run($*EXECUTABLE, $script, :out, :err);
my $out = $proc.out.slurp(:close);
$proc.err.slurp(:close);

like $out, /'END saw 10'/,
        "a module END'd qualified self-reference resolves when EVAL-loaded";
like $out, /'script END ran'/,
        'and the exit sequence continues to the remaining END phasers';

$script.IO.unlink;
