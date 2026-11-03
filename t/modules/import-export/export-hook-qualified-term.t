use v6;
use Test;
use lib 't/lib';
use QualTermUser;

# A `sub EXPORT` hook hands a class out under a package-qualified key
# (`'QualTermHook::Grid' => QualTermHook::Grid`). The routines of a module that
# `use`d the hook must keep naming it once the load is over, and the class the
# hook declares must not be nested under the importing module's package
# (Graph::RandomMaze: `Graph::Grid.new` in a `unit module Graph::RandomMaze`
# routine died with "Default constructor for 'Any' only takes named arguments").
plan 2;

is grid-name(), 'QualTermHook::Grid', 'the hook class keeps its own name in the importer';
is grid-r(3), 3, 'a routine of the importing module constructs the hook class';
