use v6;
use lib 't/lib';
use Test;

plan 17;

# An enum a loaded module's mainline declares at its top level used to leave
# its bare keys in every frame env of the importing program, because a module
# body runs in the importer's env (ADR-0084, #7817). They now live in a
# per-package table off the env. What is pinned here is that each key is
# still visible exactly where rakudo makes it visible.

use ToplevelEnumKeysBare;
use ToplevelEnumKeysUnit;

sub en($v) { $v.^name ~ '::' ~ $v.key }

# A package-less module's enum is a GLOBAL symbol: the importer sees its keys.
is TEK-A.value, 1, "a package-less module's enum key is visible to the importer";
ok TEK-B ~~ TEKSettings, '... as a value of its enum';
is en(::('TEK-A')), 'TEKSettings::TEK-A', '... and through indirect lookup';
is en(tek-a()), 'TEKSettings::TEK-A', "the module's own routine reads it";
sub reads-global-key { TEK-B }
is reads-global-key().value, 2, "a routine of the importer reads it";

# The importer's own enum shadows a module key of the same name.
{
    enum TEKLocal <TEK-B>;
    is TEK-B.^name, 'TEKLocal', "an importer's own enum key shadows the module's";
}
is TEK-B.^name, 'TEKSettings', '... only inside its own block';

# A class body's `my enum` belongs to the class, not to the importer.
is en(TEKHolder.done), 'TEKState::TEK-Done', "a class method reads its body's `my enum` key";
is en(TEKHolder.closure()()), 'TEKState::TEK-Waiting',
    '... and so does a closure it returns, called from outside the class';
is en(TEKHolder.later.list[0]), 'TEKState::TEK-Done',
    '... and a supply block tapped outside the class';
nok (try ::('TEK-Waiting')).defined, "a class body's `my enum` key is not visible to the importer";

# A unit module's private enum keys are visible to its own routines only.
is en(ToplevelEnumKeysUnit::red()), 'TEKColour::TEK-Red', "a unit module routine reads its private enum key";
is en(ToplevelEnumKeysUnit::green-closure()()), 'TEKColour::TEK-Green',
    '... and so does a closure it returns';
nok (try ::('TEK-Red')).defined, "a unit module's private enum key is not visible to the importer";
is en(ToplevelEnumKeysUnit::Inner::b()), 'TEKInner::TEK-In-B',
    "a nested module's routine reads its own enum key";

# An exported enum's keys are imported.
is en(TEK-Up), 'TEKDir::TEK-Up', "an exported enum key is imported";
is en(::('TEK-Down')), 'TEKDir::TEK-Down', '... and found by indirect lookup';
