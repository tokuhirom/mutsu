use lib 't/lib';
use Test;
use EcosystemUnitExportFixture;

plan 3;

is choose(7), 'int:7',
    'a default-imported unit-module multi keeps its Int candidate';
is choose('seven'), 'str:seven',
    'a default-imported unit-module multi keeps its Str candidate';

use EcosystemUnitExportFixture :short;
is short-choose(7), 'short:7',
    'a tagged code variable export remains callable after a unit-module import';
