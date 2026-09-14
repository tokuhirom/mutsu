use Test;

use lib 't/lib';
use NestedUnitOuter;
use NestedUnitInner;

plan 1;

is nested-probe(), 'helper',
    'a nested unit module keeps an already-loaded dependency alias visible';
