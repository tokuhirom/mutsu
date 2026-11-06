use lib 't/lib';
use Test;
use LateImportCheck;
use LateImportRoutine;

plan 4;

# `$*CU` read by a module's `sub EXPORT` before the unit's later `use` lines ran:
# the unit's call of a routine imported afterwards resolves by CHECK time.
my $r = late-routine();
my @s = late-seen();

# vim: expandtab shiftwidth=4
is $r, 42, 'the imported routine runs';
is-deeply @s[0], True, 'the call is resolved at CHECK time';
is @s[1], 'RakuAST::Declaration::Import', 'it resolves to an import';
is @s[2], 'late-routine', 'whose compile-time value is the routine';
