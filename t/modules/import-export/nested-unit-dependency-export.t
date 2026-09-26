use Test;

use lib 't/lib';
use NestedUnitOuter;
use NestedUnitInner;

plan 2;

is nested-probe(), 'helper',
    'a dependency loaded before a nested unit module stays visible to its own importer';
nok inner-sees-helper(),
    "a module does not see a routine only its dependency's own `use` imported";
