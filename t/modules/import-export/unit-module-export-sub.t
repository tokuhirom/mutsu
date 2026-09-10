use Test;

# A module's custom `sub EXPORT` hook is positional: Raku packages "the rest of
# the scope" from the `unit module` line onward, so a routine declared ABOVE it
# stays in the compunit's outer scope. That is the only place `sub EXPORT` can
# live, because `use` looks it up there.
#
# mutsu switched the runtime package for the WHOLE compilation unit before the
# sub-hoist pass, so the hook registered as `Foo::EXPORT` and the `GLOBAL::EXPORT`
# lookup missed it -- the custom export of every `unit module` was silently never
# called. Measured against rakudo (2026-09-09), which runs the above-the-line
# form and leaves the below-the-line one alone.
plan 5;

use lib 't/lib';
use UnitModuleExportSub;
use UnitModuleExportSubBelow;

is MyAlias.^name, 'Int',
    'a custom EXPORT above the `unit module` line installs its type object';
is exported-by-export(), 'from EXPORT',
    '...and its routines';
ok ::('&not-exported') ~~ Failure,
    'a custom EXPORT replaces the default import: `our sub` is not pulled in';

is plain-export(), 'ordinary export',
    'a `sub EXPORT` BELOW the `unit module` line is not the hook: normal exports still happen';
ok ::('BelowAlias') ~~ Failure,
    '...and the Map it would have returned is not installed';
