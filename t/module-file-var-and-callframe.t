use Test;
use lib 't/lib';
use FileVarFixture;

# `$?FILE` is a compile-time constant of the compilation unit it is written in.
# It used to be read out of the runtime environment, which only tracked the unit
# being *loaded*, so once a module's mainline had finished every routine in it
# reported the main script instead of the module. `callframe(N).file` had the
# matching gap: it read the same runtime value rather than the file the frame's
# code was defined in.
#
# Together these defeat the standard "walk out of my own file to find who called
# me" shape a test framework uses to report a failure's location — see
# todo/tickets/vendor-real-test-module.md.
#
# Paths are compared with `.contains` rather than `eq`: rakudo spells $?FILE
# absolutely and appends the module name (`/abs/Foo.rakumod (Foo)`), so this
# file has to run under both.

plan 11;

my $script = 't/module-file-var-and-callframe.t';
my $module = 't/lib/FileVarFixture.rakumod';

ok $?FILE.contains($script), 'the main script reports its own file';

ok fixture-file().contains($module),
    'a module routine reports the module file, not the script';
nok fixture-file().contains($script),
    'and specifically not the script it was called from';

ok fixture-file-interpolated().contains($module),
    'an interpolated $?FILE reports the module file too';

ok fixture-file-nested().contains($module),
    'and so does a module routine called from another module routine';

sub script-file() { $?FILE }
ok fixture-invokes(&script-file).contains($script),
    'a script routine invoked from the module still reports the script';

ok fixture-frame-file().contains($module),
    'callframe reports the module file for a frame running in the module';

sub script-frame-file() { callframe(1).file }
sub script-outer() { script-frame-file() }
ok script-outer().contains($script),
    'callframe still reports the script for a frame running in the script';

# The whole point of the pair: a module routine can tell "am I still inside my
# own file?" and stop at the first frame that is not — which is how rakudo's
# Test.rakumod attributes a failing assertion to the test script.
ok fixture-caller-outside().contains($script),
    'a module walking out of its own file lands on the calling script';

sub calls-the-walker() { fixture-caller-outside() }
ok calls-the-walker().contains($script),
    'and lands on it through an intervening script routine too';

ok fixture-invokes(-> { fixture-caller-outside() }).contains($script),
    'a block written in this file is attributed here, not to its invoker';
