use v6;
use Test;

# A module load must not discard its writes to a dynamic variable that belongs
# to the IMPORTING scope. Both halves of a load were affected: the mainline and
# `sub EXPORT` each ran in the caller's env and had that env restored wholesale
# afterwards, which reverted `$*x` along with the module's own lexicals.
#
# `$*` state set by an importer is a normal way for a module to report a
# load-time fact -- `modules/if/`'s upstream `t/if.rakutest` counts loads
# exactly this way, and three of its five assertions failed on this alone.
# https://github.com/tokuhirom/mutsu/issues/8229
#
# The loads go through EVAL so the `use` really happens inside the dynamic
# scope; a plain top-level `use` runs at BEGIN, before the `my $*...`
# assignment.

plan 6;

use lib 't/lib/Issue8229';

{
    my $*PACKAGE_LOADED = 0;
    EVAL q[use DynCountMainline];
    is $*PACKAGE_LOADED, 1, "a module mainline's write to a caller dynamic survives the load";
}

{
    my $*PACKAGE_LOADED = 0;
    EVAL q[use DynCountExport];
    is $*PACKAGE_LOADED, 1, "a `sub EXPORT`'s write to a caller dynamic survives the load";
}

# Raku runs `sub EXPORT` on every import, not once per process, so a re-`use`
# of an already-loaded module counts again. That re-run reads the module's
# remembered scope, which must not shadow the importer's *live* dynamic.
{
    my $*PACKAGE_LOADED = 0;
    EVAL q[use DynCountExport];
    EVAL q[use DynCountExport];
    is $*PACKAGE_LOADED, 2, 'a re-`use` re-runs EXPORT and counts against the live dynamic';
}

# The carry-back is keyed on the dynamic, not on the loading frame: a nested
# dynamic scope sees only its own binding change. A module mainline runs once
# per process, so this needs a module no earlier assertion has loaded.
{
    my $*PACKAGE_LOADED = 10;
    {
        my $*PACKAGE_LOADED = 0;
        EVAL q[use DynCountNested];
        is $*PACKAGE_LOADED, 1, 'the innermost dynamic binding is the one that is written';
    }
    is $*PACKAGE_LOADED, 10, 'an outer dynamic binding of the same name is untouched';
}

# Only dynamics are carried back. `sub EXPORT`'s own lexicals must still die
# with the call -- dropping them is what the wholesale env restore was there
# for, and narrowing it must not turn EXPORT's scratch variables into writes
# the importer sees.
{
    my $*PACKAGE_LOADED = 0;
    my $export-local = 'caller';
    EVAL q[use DynCountExportLocal];
    is $export-local, 'caller', "a `sub EXPORT` lexical does not leak into the importer";
}
