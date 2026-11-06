use lib 't/lib';
use Test;
use BodiedProtoUnitFile;

plan 2;

# A `proto` with its own body and no candidates, exported from a `unit module`
# whose name differs from its file, is imported like any exported routine.
is bodied-proto-sel(5, 1, 2), 'item=5 at 1,2', 'bodied proto is imported';
throws-like { bodied-proto-sel(5) }, Exception, 'and its body runs', message => /'need a position'/;
