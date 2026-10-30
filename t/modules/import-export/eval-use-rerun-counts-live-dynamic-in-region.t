use Test;
use MONKEY-SEE-NO-EVAL;
use lib 't/lib/Issue8229';

# An `EVAL` whose unit imports runs over an import region (a tier of its own).
# A re-`use` of a loaded module re-runs its `sub EXPORT` against the live
# dynamic variables of the importer's frame, which an outer frame declared:
# the region must not hide them, or the slang `if` stacks one more handler per
# EVAL and loads the module once more each time (ADR-14140). The first two
# tests are checked against rakudo; `use if` is the bundled battery's module,
# which rakudo has no copy of in this tree.
plan 4;

sub count-loads(Str $code) {
    my $*PACKAGE_LOADED = 0;
    EVAL $code;
    $*PACKAGE_LOADED
}

is count-loads('use DynCountExport'), 1, 'a use counts once';
is count-loads('use DynCountExport'), 1, 'a re-use in a fresh frame counts once again';
for 1..2 {
    is count-loads('use if; use DynCountExport:if(1)'), 1,
        "a use with `:if` counts once on EVAL $_ (no handler stacks up)";
}
