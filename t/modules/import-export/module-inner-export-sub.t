use v6;
use lib 't/lib';
use Test;
use InnerExportUser;
use InnerExportUser2 'a', 'b';

# The Slangify pattern: a module's EXPORT map can export an `&EXPORT` sub
# into the *using module*, which then acts as that module's own EXPORT for
# its importers — called with the end user's `use` arguments. Slangify uses
# exactly this to install slangs at the user's compile time (ADR-0026).

plan 2;

is p1(), 'provided-by-X1', 'imported &EXPORT ran as the using module EXPORT';
is p2(), 'provided-by-X2-a-b', 'end-user use arguments reach the inner EXPORT';
