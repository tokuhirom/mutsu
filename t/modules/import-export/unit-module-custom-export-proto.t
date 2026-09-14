use Test;

use lib 't/lib';
use UnitModuleCustomExportProto;

plan 2;

is custom-unit-marker(), 'custom',
    'a unit module still installs the custom EXPORT map';
is exported-family('ok'), 'family:ok',
    'a custom EXPORT hook keeps the exported proto multi family importable';
