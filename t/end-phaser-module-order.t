use v6;
use Test;

# END phasers run in reverse *install* order, and rakudo installs a module's
# when the `use` is compiled -- before anything the loading compunit declares.
# So a script's own END runs first and the module's last.
#
# mutsu registers the main compunit's top-level ENDs eagerly, before the body
# runs (so they still run when the body dies), which put them *ahead* of every
# module's in the registration list and reversed the pair. Under the real
# Test.rakumod that is not cosmetic: the module's END is the plan check, so it
# ran before the file's own `END { is ... }` and reported "You planned 2 tests,
# but ran 1" on a file that went on to emit both.

plan 5;

my $dir = $*TMPDIR.child("mutsu-end-order-{$*PID}");
$dir.mkdir;
END { try { .unlink for $dir.dir; $dir.rmdir } }

sub run-snippet($name, $source) {
    my $file = $dir.child($name);
    $file.spurt($source);
    my $proc = run($*EXECUTABLE, $file.absolute, :out, :err);
    my $out = $proc.out.slurp(:close);
    $proc.err.slurp(:close);
    $out.trim.subst("\n", " ", :g)
}

$dir.child('EndOrderA.rakumod').spurt(
    'unit module EndOrderA;
END { say "A" }
');
$dir.child('EndOrderB.rakumod').spurt(
    'unit module EndOrderB;
END { say "B" }
');
# No `use lib` here: rakudo rejects one inside a module ("cannot be
# precompiled"). The loading script's `use lib` is what puts the directory on
# the search path for this nested `use` too.
$dir.child('EndOrderNest.rakumod').spurt(
    'unit module EndOrderNest;
use EndOrderA;
END { say "N" }
');

my $lib = 'use lib "' ~ $dir.absolute ~ '";' ~ "\n";

is run-snippet('one.raku', $lib ~ 'use EndOrderA;
END { say "script" }
'), 'script A', "the script's END runs before the module's";

is run-snippet('two.raku', $lib ~ 'use EndOrderA;
use EndOrderB;
END { say "script" }
'), 'script B A',
    'later-loaded modules run before earlier-loaded ones';

# A module used by a module installs its END first, so it runs last. Asserted
# on the tail only: rakudo precompiles EndOrderNest in a separate phase that
# loads EndOrderA and fires its END there too, so raku's own output for this
# snippet carries a leading stray "A" that has nothing to do with the ordering.
like run-snippet('nested.raku', $lib ~ 'use EndOrderNest;
END { say "script" }
'), / 'script N A' $ /,
    'a module used by a module installs its END first, so it runs last';

# Two ENDs in the same compunit are unaffected: still reverse source order.
is run-snippet('same.raku', 'END { say "first" }
END { say "second" }
'), 'second first', 'two ENDs in one compunit still run in reverse';

# The eager registration this ordering has to preserve: an END still runs when
# the mainline dies before reaching the end of the file.
is run-snippet('dies.raku', $lib ~ 'use EndOrderA;
END { say "script" }
die "boom";
'), 'script A', 'the order survives a mainline that dies';
